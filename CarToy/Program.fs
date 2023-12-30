open System

[<EntryPoint>]

let main _ =
    Console.ReadLine()
    |> int
    |> fun numberLength ->
        Console.ReadLine()
        |> Seq.take numberLength
        |> Seq.fold
            (fun (speed, distance) button ->
                ((if button = '+' then speed + 1 else max 0 (speed - 1)), distance + speed))
            (0, 0)
        |> snd
        |> printfn "%d"

    0
