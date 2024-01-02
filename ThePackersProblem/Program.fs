open System

type BoxOperation =
    | Open of int
    | Close of int

let solveProblem operations =
    let mutable stack = [ 0 ]
    let mutable maxDepth = 0
    let mutable hasError = false

    for operation in operations do
        match operation with

        | Open size ->
            if size <= List.head stack || hasError then
                hasError <- true // Can't open a larger box inside a smaller one
            else
                stack <- size :: stack
                maxDepth <- max maxDepth (List.length stack - 1)

        | Close size ->
            if size = List.head stack then
                hasError <- true // Can't close a box different in size from the top box on the stack
            else
                stack <- List.tail stack

    if hasError || List.length stack > 1 then
        "You are fired!"
    else
        "Good Job! " + string maxDepth

[<EntryPoint>]
let main _ =
    let n = Console.ReadLine() |> int

    let operations =
        [ for _ in 1 .. (n * 2) do
              let line = Console.ReadLine().Split(' ')
              let character, size = line[0], line[1] |> int

              if character = "0" then yield Open size
              elif character = "C" then yield Close size
              else failwith "Invalid operation!" ]

    let result = solveProblem operations
    printfn "%s" result

    0 // return an integer exit code
