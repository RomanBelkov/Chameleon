open System
open Trik
open Microsoft.FSharp.Core.Operators
open Microsoft.FSharp.Control

type RGB = {r: int; g: int; b: int} with
    static member Zero = {r = 0; g = 0; b =0}

[<Literal>]
let MagicSize = 4

let sleep (msec:int) = System.Threading.Thread.Sleep msec

[<EntryPoint>]
let main args =
    Helpers.I2C.init "/dev/i2c-2" 0x48 1
    use gnd = new Trik.PowerMotor(0x16)
    use stripe = new Trik.LedStripe(0x14, 0x15, 0x17, 0x16)
    gnd.SetPower(100)

    use gSM = new Servomotor("/sys/class/pwm/ehrpwm.1:1", {stop = 0; zero = 1100000; min = 900000; max = 1400000; period = 20000000})

    use file = new IO.StreamReader("/tmp/dsp-detector.out.fifo")

    let mutable turnG = 0
    let mutable turnGprev = 0

    // Для заполнения flagsMas[] и red[], gre[], blu[]
    let createMagicArray() = Array.create MagicSize RGB.Zero

    let currInput = ref (createMagicArray ())

    let rec updateCurrentFrameInfo () =
        async {
            let line = file.ReadLine() // hope we are faster to read than to process frames with DSP
            let mas = line.Split [|' '|]
            let tmp = Array.init MagicSize <| fun i -> 
                { r = 0xFF &&& (int mas.[i + 3] >>> 16)
                  g = 0xFF &&& (int mas.[i + 3] >>> 8)
                  b = 0xFF &&& int mas.[i + 3] }

            currInput := tmp // no explicit lock 
            do! updateCurrentFrameInfo()
        } 

    updateCurrentFrameInfo() |> Async.Start

    let prevLED = ref RGB.Zero

    // Для определения следующего цвета ленты
    let mutable nextObtained = createMagicArray()
    let mutable prevObtained = createMagicArray()

    // red, gre, blu are value from 0 to 255
    let changeColor prev cur =
        let deltaR = cur.r - prev.r
        let deltaG = cur.g - prev.g
        let deltaB = cur.b - prev.b
        let steps = 10
        let sleep () = sleep <| 2000 / steps
        for i = 1 to steps do        
            let red, green, blue = (prev.r * steps + i * deltaR) / steps, (prev.g * steps + i * deltaG) / steps, (prev.b * steps + i * deltaB) / steps
            stripe.SetPower(red, green, blue)
            sleep()

    gSM.SetPower(0)

    while true do

        let flags = Array.create MagicSize 0

        prevObtained <- nextObtained

        let steps = 10

        let sleep() = sleep <| 2000 / steps

        for counter = 1 to steps do
            let prevInput = currInput  
            let threshold = 10
            for i = 1 to 3 do
            
                if Array.forall2 (fun x y -> Math.Abs(x.r - y.r) <= threshold && Math.Abs(x.g - y.g) <= threshold && Math.Abs(x.b - y.b) <= threshold) !currInput !prevInput
                then     
                    flags.[i] <- flags.[i] + 1

                    nextObtained <- !currInput

        let inline change (rgb: RGB) =
            printfn "change (redPrev, grePrev, bluPrev) %A" prevLED
            printfn "change (red, gre, blu) %A" (rgb.r, rgb.g, rgb.b)
            changeColor !prevLED rgb
            prevLED := rgb

        let mutable changeOfColor1 = Array.create MagicSize 0
        let mutable changeOfColor2 = Array.create MagicSize 0

        if Math.Abs(nextObtained.[1].r - prevObtained.[1].r) >= 10 || 
           Math.Abs(nextObtained.[1].g - prevObtained.[1].g) >= 10 || 
           Math.Abs(nextObtained.[1].b - prevObtained.[1].b) >= 10 
           then changeOfColor1.[1] <- 1

        if Math.Abs(nextObtained.[2].r - prevObtained.[2].r) >= 10 || 
           Math.Abs(nextObtained.[2].g - prevObtained.[2].g) >= 10 || 
           Math.Abs(nextObtained.[2].b - prevObtained.[2].b) >= 10 
           then changeOfColor1.[2] <- 1

        if Math.Abs(nextObtained.[3].r - prevObtained.[3].r) >= 10 || 
           Math.Abs(nextObtained.[3].g - prevObtained.[3].g) >= 10 || 
           Math.Abs(nextObtained.[3].b - prevObtained.[3].b) >= 10 
           then changeOfColor1.[3] <- 1
    
        if Math.Abs(nextObtained.[1].r - (!prevLED).r) >= 10 || 
           Math.Abs(nextObtained.[1].g - (!prevLED).g) >= 10 || 
           Math.Abs(nextObtained.[1].b - (!prevLED).b) >= 10 
           then changeOfColor2.[1] <- 1

        if Math.Abs(nextObtained.[2].r - (!prevLED).r) >= 10 || 
           Math.Abs(nextObtained.[2].g - (!prevLED).g) >= 10 || 
           Math.Abs(nextObtained.[2].b - (!prevLED).b) >= 10 
           then changeOfColor2.[2] <- 1

        if Math.Abs(nextObtained.[3].r - (!prevLED).r) >= 10 || 
           Math.Abs(nextObtained.[3].g - (!prevLED).g) >= 10 || 
           Math.Abs(nextObtained.[3].b - (!prevLED).b) >= 10 
           then changeOfColor2.[3] <- 1

    // 9 попаданий в один цвет из 12-ти за 2 секунды => он стабилен
        if flags.[1] >= 9 && flags.[2] >= 9 && flags.[3] >= 9 then

            // Цвет изменился в этой области и относительно текущего
            if changeOfColor1.[1] = 1 && changeOfColor2.[1] = 1 && turnG <> -90 then
                turnG <- -90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[1]
            elif changeOfColor1.[2] = 1 && changeOfColor2.[2] = 1 && turnG <> 0 then
                turnG <- 0
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[2]
            elif changeOfColor1.[3] = 1 && changeOfColor2.[3] = 1 && turnG <> 90 then
                turnG <- 90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[3]

            // Если цвет изменился только в этой области
            elif changeOfColor1.[1] = 1 && turnG <> -90 then
                turnG <- -90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[1]
            elif changeOfColor1.[2] = 1 && turnG <> 0 then
                turnG <- 0
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[2]
            elif changeOfColor1.[3] = 1 && turnG <> 90 then
                turnG <- 90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[3]

            // Цвет изменился только относительно текущего
            elif changeOfColor2.[1] = 1 && turnG <> -90 then
                turnG <- -90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[1]
            elif changeOfColor2.[2] = 1 && turnG <> 0 then
                turnG <- 0
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[2]
            elif changeOfColor2.[3] = 1 && turnG <> 90 then
                turnG <- 90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[3]

            // Цвет изменился в этой области и относительно текущего
            elif changeOfColor1.[1] = 1 && changeOfColor2.[1] = 1 then
                change nextObtained.[1]
            elif changeOfColor1.[2] = 1 && changeOfColor2.[2] = 1 then
                change nextObtained.[2]
            elif changeOfColor1.[3] = 1 && changeOfColor2.[3] = 1 then
                change nextObtained.[3]

            // Цвет изменился в этой области
            elif changeOfColor1.[1] = 1 then
                change nextObtained.[1]
            elif changeOfColor1.[2] = 1 then
                change nextObtained.[2]
            elif changeOfColor1.[3] = 1 then
                change nextObtained.[3]

            // Цвет отличен от текущего
            elif changeOfColor2.[1] = 1 then
                change nextObtained.[1]
            elif changeOfColor2.[2] = 1 then
                change nextObtained.[2]
            elif changeOfColor2.[3] = 1 then
                change nextObtained.[3]

            // Цвет везде одинаковый и нигде не изменился
            elif turnG <> -90 then
                turnG <- -90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
            elif turnG <> 0 then
                turnG <- 0
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
            elif turnG <> 90 then
                turnG <- 90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG

        // Цвет стабилен только в первых двух областях 
        elif flags.[1] >= 9 && flags.[2] >= 9 then

            // Цвет изменился в этой области и относительно текущего
            if changeOfColor1.[1] = 1 && changeOfColor2.[1] = 1 && turnG <> -90 then
                turnG <- -90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[1]
            elif changeOfColor1.[2] = 1 && changeOfColor2.[2] = 1 && turnG <> 0 then
                turnG <- 0
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[2]

            // Цвет изменился в этой области
            elif changeOfColor1.[1] = 1 && turnG <> -90 then
                turnG <- -90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[1]
            elif changeOfColor1.[2] = 1 && turnG <> 0 then
                turnG <- 0
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[2]

            // Цвет отличен от текущего
            elif changeOfColor2.[1] = 1 && turnG <> -90 then
                turnG <- -90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[1]
            elif changeOfColor2.[2] = 1 && turnG <> 0 then
                turnG <- 0
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[2]


            // Цвет изменился в этой области и относительно текущего
            elif changeOfColor1.[1] = 1 && changeOfColor2.[1] = 1 then
                change nextObtained.[1]
            elif changeOfColor1.[2] = 1 && changeOfColor2.[2] = 1 then
                change nextObtained.[2]

            // Цвет изменился в этой области
            elif changeOfColor1.[1] = 1 then
                change nextObtained.[1]
            elif changeOfColor1.[2] = 1 then
                change nextObtained.[2]

            // Цвет отличен от текущего
            elif changeOfColor2.[1] = 1 then
                change nextObtained.[1]
            elif changeOfColor2.[2] = 1 then
                change nextObtained.[2]


            // Цвет везде одинаковый и нигде не изменился
            if turnG <> 90 then
                turnG <- 90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
            elif turnG <> 0 then
                turnG <- 0
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG

        // Цвет стабилен только в 1 и 3 областях 
        elif flags.[1] >= 9 && flags.[3] >= 9 then

            // Цвет изменился в этой области и относительно текущего
            if changeOfColor1.[1] = 1 && changeOfColor2.[1] = 1 && turnG <> -90 then
                turnG <- -90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[1]
            elif changeOfColor1.[3] = 1 && changeOfColor2.[3] = 1 && turnG <> 90 then
                turnG <- 90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[3]

            // Цвет изменился в этой области
            elif changeOfColor1.[1] = 1 && turnG <> -90 then
                turnG <- -90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[1]
            elif changeOfColor1.[3] = 1 && turnG <> 90 then
                turnG <- 90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[3]

            // Цвет отличен от текущего
            elif changeOfColor2.[1] = 1 && turnG <> -90 then
                turnG <- -90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[1]
            elif changeOfColor2.[3] = 1 && turnG <> 90 then
                turnG <- 90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[3]

            // Цвет изменился в этой области и относительно текущего
            elif changeOfColor1.[1] = 1 && changeOfColor2.[1] = 1 then
                change nextObtained.[1]
            elif changeOfColor1.[3] = 1 && changeOfColor2.[3] = 1 then
                change nextObtained.[3]

            // Если цвет изменился в этой области, то все ОК
            elif changeOfColor1.[1] = 1 then
                change nextObtained.[1]
            elif changeOfColor1.[3] = 1 then
                change nextObtained.[3]

            // Если цвет отличен от текущего, то все ОК
            elif changeOfColor2.[1] = 1 then
                change nextObtained.[1]
            elif changeOfColor2.[3] = 1 then
                change nextObtained.[2]


            // Цвет везде одинаковый и нигде не изменился
            elif turnG <> -90 then
                turnG <- -90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
            elif turnG <> 90 then
                turnG <- 90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG

        // Цвет стабилен только в 2 и 3 областях 
        elif flags.[2] >= 9 && flags.[3] >= 9 then

            // Цвет изменился в этой области и относительно текущего
            if changeOfColor1.[2] = 1 && changeOfColor2.[2] = 1 && turnG <> 0 then
                turnG <- 0
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[2]
            elif changeOfColor1.[3] = 1 && changeOfColor2.[3] = 1 && turnG <> 90 then
                turnG <- 90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[3]

            // Цвет изменился в этой области
            elif changeOfColor1.[2] = 1 && turnG <> 0 then
                turnG <- 0
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[2]
            elif changeOfColor1.[3] = 1 && turnG <> 90 then
                turnG <- 90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[3]

            // Цвет отличен от текущего
            elif changeOfColor2.[2] = 1 && turnG <> 0 then
                turnG <- 0
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[2]
            elif changeOfColor2.[3] = 1 && turnG <> 90 then
                turnG <- 90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[3]

            // Цвет изменился в этой области и относительно текущего
            elif changeOfColor1.[2] = 1 && changeOfColor2.[2] = 1 then
                change nextObtained.[2]
            elif changeOfColor1.[3] = 1 && changeOfColor2.[3] = 1 then
                change nextObtained.[3]

            // Цвет изменился в этой области
            elif changeOfColor1.[2] = 1 then
                change nextObtained.[2]
            elif changeOfColor1.[3] = 1 then
                change nextObtained.[3]

            // Цвет отличен от текущего
            elif changeOfColor2.[2] = 1 then
                change nextObtained.[2]
            elif changeOfColor2.[3] = 1 then
                change nextObtained.[3]

            // Цвет везде одинаковый и нигде не изменился
            elif turnG <> 0 then
                turnG <- 0
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
            elif turnG <> 90 then
                turnG <- 90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG

        // Цвет стабилен только в 1 области
        elif flags.[1] > 9 then
            if turnG <> -90 then
                turnG <- -90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
            if changeOfColor2.[1] = 1 then
                change nextObtained.[1]

        // Цвет стабилен только во 2 области
        elif flags.[2] > 9 then
            if turnG <> 0 then
                turnG <- 0
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
            if changeOfColor2.[2] = 1 then
                change nextObtained.[2]

        // Цвет стабилен только в 3 области
        elif flags.[3] > 9 then
            if turnG <> 90 then
                turnG <- 90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
            if changeOfColor2.[3] = 1 then
                change nextObtained.[3]
    0
        