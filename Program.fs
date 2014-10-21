open System
open Trik

type RGB = {r: int; g: int; b: int} with
    static member Zero = {r = 0; g = 0; b = 0}

[<Literal>]
let MagicSize = 4
let PixelChange = 10
let steps = 80

[<EntryPoint>]
let main args =
    Trik.Helpers.I2C.Init "/dev/i2c-2" 0x48 1
    use stripe = new Trik.LedStripe({Red = 0x14; Green = 0x15; Blue = 0x16; Ground = 0x17;})
    use gSM = new ServoMotor("/sys/class/pwm/ehrpwm.1:1", {stop = 0; zero = 1550000; min = 800000; max = 2300000; period = 20000000})

    //starting & configuring camera daemon
    Helpers.SendToShell "/etc/init.d/mxn-sensor-webcam.sh start"
    Helpers.SendToShell """echo "mxn 4 4" > /run/mxn-sensor.in.fifo"""

    //fifo with color data
    use file = new IO.StreamReader("/run/mxn-sensor.out.fifo")

    //Button exit
    let buttonPad = new ButtonPad("/dev/input/event0") 
    buttonPad.Start()
    let buttonPressed = buttonPad.CheckPressing ButtonEventCode.Down

    //stuff we need
    let mutable turnG = 0
    let prevLED = ref RGB.Zero

    let createMagicArray() = Array.create MagicSize RGB.Zero // Для заполнения flagsMas[] и red[], gre[], blu[]
    let currInput = ref (createMagicArray ())
    let mutable nextObtained = createMagicArray()
    let mutable prevObtained = createMagicArray()

    let flags = Array.create MagicSize 0 //flags create were in looop
    let mutable changeOfColor = Array.create MagicSize 0

    file.ReadLine() |> ignore //dumping first line
    let rec updateCurrentFrameInfo() =
        async {
            let line = file.ReadLine() // hope we are faster to read than to process frames with DSP
            let mas = line.Split [|' '|]
            if mas.[0] = "color:" then
                let tmp = Array.init MagicSize (fun i -> 
                    { r = 0xFF &&& (int mas.[i + 3] >>> 16)
                      g = 0xFF &&& (int mas.[i + 3] >>> 8)
                      b = 0xFF &&& int mas.[i + 3] })
                currInput := tmp // no explicit lock 
                else ()
            do! updateCurrentFrameInfo()
        } 
    let cts = new System.Threading.CancellationTokenSource() // we want to close async as we exit application
    Async.Start(updateCurrentFrameInfo(), cts.Token)
   
    // color changer
    let changeColor prev curr =
        let deltaR = curr.r - prev.r
        let deltaG = curr.g - prev.g
        let deltaB = curr.b - prev.b
        for i = 1 to steps do 
            let smoothChange = (i / steps)       
            stripe.SetPower(prev.r + smoothChange * deltaR, prev.g + smoothChange * deltaG, (prev.b + smoothChange * deltaB) * 100 / 85)
            System.Threading.Thread.Sleep (800 / steps)


    let inline change (rgb: RGB) = 
        //printfn "change (redPrev, grePrev, bluPrev) %A" prevLED
        //printfn "change (red, gre, blu) %A" (rgb.r, rgb.g, rgb.b) 
        changeColor !prevLED rgb
        prevLED := rgb

    gSM.SetPower(turnG)

    while not !buttonPressed do

        Array.fill flags 0 MagicSize 0
        Array.fill changeOfColor 0 MagicSize 0

        prevObtained <- nextObtained

        for counter = 1 to 12 do
            let prevInput = currInput 
            for i = 1 to 3 do
                if Array.forall2 (fun x y -> Math.Abs(x.r - y.r) <= PixelChange && 
                                             Math.Abs(x.g - y.g) <= PixelChange && 
                                             Math.Abs(x.b - y.b) <= PixelChange) !currInput !prevInput
                then     
                    flags.[i] <- flags.[i] + 1
            nextObtained <- !currInput

        //PixelChange is previos magic const 10
        for i in 1 .. 3 do
            if Math.Abs(nextObtained.[i].r - prevObtained.[i].r) >= PixelChange || 
               Math.Abs(nextObtained.[i].g - prevObtained.[i].g) >= PixelChange || 
               Math.Abs(nextObtained.[i].b - prevObtained.[i].b) >= PixelChange 
               then changeOfColor.[i] <- 1

    // 9 попаданий в один цвет из 12-ти за 2 секунды => он стабилен
        if flags.[1] >= 9 && flags.[2] >= 9 && flags.[3] >= 9 then

            // Если цвет изменился только в этой области
            if changeOfColor.[1] = 1 && turnG <> -90 then
                turnG <- -90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[1]
            elif changeOfColor.[2] = 1 && turnG <> 0 then
                turnG <- 0
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[2]
            elif changeOfColor.[3] = 1 && turnG <> 90 then
                turnG <- 90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[3]

            // Цвет изменился в этой области
            elif changeOfColor.[1] = 1 then
                change nextObtained.[1]
            elif changeOfColor.[2] = 1 then
                change nextObtained.[2]
            elif changeOfColor.[3] = 1 then
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

            // Цвет изменился в этой области
            if changeOfColor.[1] = 1 && turnG <> -90 then
                turnG <- -90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[1]
            elif changeOfColor.[2] = 1 && turnG <> 0 then
                turnG <- 0
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[2]

            // Цвет изменился в этой области
            elif changeOfColor.[1] = 1 then
                change nextObtained.[1]
            elif changeOfColor.[2] = 1 then
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

            // Цвет изменился в этой области
            if changeOfColor.[1] = 1 && turnG <> -90 then
                turnG <- -90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[1]
            elif changeOfColor.[3] = 1 && turnG <> 90 then
                turnG <- 90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[3]

            // Если цвет изменился в этой области, то все ОК
            elif changeOfColor.[1] = 1 then
                change nextObtained.[1]
            elif changeOfColor.[3] = 1 then
                change nextObtained.[3]

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

            // Цвет изменился в этой области
            if changeOfColor.[2] = 1 && turnG <> 0 then
                turnG <- 0
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[2]
            elif changeOfColor.[3] = 1 && turnG <> 90 then
                turnG <- 90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
                change nextObtained.[3]

            // Цвет изменился в этой области
            elif changeOfColor.[2] = 1 then
                change nextObtained.[2]
            elif changeOfColor.[3] = 1 then
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
        elif flags.[1] >= 9 then
            if turnG <> -90 then
                turnG <- -90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
            if changeOfColor.[1] = 1 then
                change nextObtained.[1]

        // Цвет стабилен только во 2 области
        elif flags.[2] >= 9 then
            if turnG <> 0 then
                turnG <- 0
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
            if changeOfColor.[2] = 1 then
                change nextObtained.[2]

        // Цвет стабилен только в 3 области
        elif flags.[3] >= 9 then
            if turnG <> 90 then
                turnG <- 90
                System.Threading.Thread.Sleep 1000
                gSM.SetPower(turnG)
                printfn "\ngSM.SetPower(%A)\n" turnG
            if changeOfColor.[3] = 1 then
                change nextObtained.[3]
    
    cts.Cancel()
    file.Close()
    Helpers.SendToShell "/etc/init.d/mxn-sensor-webcam.sh stop"
    0
        