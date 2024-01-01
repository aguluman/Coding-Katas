open System

let readInputFields (fieldNumMsg:string) :string =
    printfn $"Enter the number of words for the {fieldNumMsg} column"
    let columnLength = Console.ReadLine() |> int
    printfn $"Enter the words for the {fieldNumMsg} column"
    let text = [ for _ in 1..columnLength -> Console.ReadLine() ] |> String.concat " "
    text

let removeConsecutiveDuplicates (input: string): string =
        Seq.pairwise input
        |> Seq.fold (fun acc (a, b) -> if a <> b then acc + string(b) else acc) (string(input.[0]))

let longestCommonSubSequence (str1: string) (str2: string) :string =
    let m, n = str1.Length, str2.Length
    let L = Array2D.create (m + 1) (n + 1) 0

    for i in 0..m do
        for j in 0..n do
            L[i, j] <-
                match i, j with
                | 0, _
                | _, 0 -> 0
                | _ when str1[i - 1] = str2[j - 1] -> L[i - 1, j - 1] + 1
                | _ -> max L[i - 1, j] L[i, j - 1]

    let rec constructLCS i j lcs =
        match i, j with
        | 0, _ | _, 0 -> new string (Array.ofList lcs) //If we've reached the start of either sequence
        | _ when str1.[i - 1] = str2.[j - 1] -> // If the characters match
            constructLCS (i - 1) (j - 1) (str1.[i - 1] :: lcs) // Add the character to the LCS and move diagonally
        | _ when L.[i - 1, j] >= L.[i, j - 1] -> constructLCS (i - 1) j lcs // If the character from str1 doesn't match, move upwards
        | _ -> constructLCS i (j - 1) lcs // If the character from str2 doesn't match, move to the left

    constructLCS m n []

let decodeDirective (text1: string) (text2: string) : string =
    match longestCommonSubSequence text1 text2 with
    | "" -> "-"
    | lcs -> lcs

[<EntryPoint>]
let main _ =
    let text1 = readInputFields "first"
    let text2 = readInputFields "second"

    let directive = decodeDirective text1 text2
    let directiveNoDuplicates = removeConsecutiveDuplicates directive

    Console.WriteLine($"Decoded Directive is : {directiveNoDuplicates}")
    0
