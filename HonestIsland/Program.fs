open System

let getAllPossibleNumbersOfLiars (repliesUnsorted: int list) : int list =
    // Sort the replies
    let replies = List.sort repliesUnsorted

    // Define a function to test whether a reply might indicate the count of liars
    let isPossibleLiarCount i r =
        if i <= r && (i = 0 || r <> List.item (i - 1) replies) then
            Some i
        else
            None

    // Map the replies to possible liar counts (or None where not possible)
    let possibleCounts = List.mapi isPossibleLiarCount replies

    // Extract the counts (ignoring the Nones)
    let possibleCountsValues = List.choose id possibleCounts

    // Convert counts to a set
    let possibleNumberOfLiars = Set.ofList possibleCountsValues

    // When all residents can be liars.
    let totalResidents = List.length repliesUnsorted

    if replies |> List.last < totalResidents then
        Set.add totalResidents possibleNumberOfLiars |> Set.toList |> List.sort
    else
        possibleNumberOfLiars |> Set.toList |> List.sort

[<EntryPoint>]
let main _ =
    printfn "Enter the claims from each resident separated by space:"
    let repliesUnsorted = Console.ReadLine().Split(' ') |> Array.map int |> Array.toList
    let results = getAllPossibleNumbersOfLiars repliesUnsorted
    printfn $"%A{results}"
    0
