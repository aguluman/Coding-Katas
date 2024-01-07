open System

let getAllPossibleNumbersOfLiars (repliesUnsorted: int list) : int list =
    let replies = List.sort repliesUnsorted

    let possibleNumberOfLiars =
        replies
        |> List.mapi (fun i r -> (i, r))
        |> List.fold
            (fun acc (i, r) ->
                if i <= r && (i = 0 || r <> List.item (i - 1) replies) then
                    Set.add i acc
                else
                    acc)
            Set.empty

    if List.last replies < List.length repliesUnsorted then
        Set.add (List.length repliesUnsorted) possibleNumberOfLiars
    else
        possibleNumberOfLiars
    |> Set.toList
    |> List.sort

[<EntryPoint>]
let main _ =
    printfn "Enter the claims from each resident separated by space:"
    let repliesUnsorted = Console.ReadLine().Split(' ') |> Array.map int |> Array.toList
    let results = getAllPossibleNumbersOfLiars repliesUnsorted
    printfn $"%A{results}"
    0
