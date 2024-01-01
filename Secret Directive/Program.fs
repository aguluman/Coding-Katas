open System

let longestCommonSubSequence (str1: string) (str2: string) : string =
    let array1 = str1.ToCharArray()
    let array2 = str2.ToCharArray()

    let m = array1.Length
    let n = array2.Length

    let L = Array2D.create (m + 1) (n + 1) 0

    for i in 0..m do
        for j in 0..n do
            if i = 0 || j = 0 then
                L[i,j] <- 0
            elif array1[i - 1] = array2[j - 1] then
                L[i,j] <- L[i - 1, j - 1] + 1
            else
                L[i, j] <- max L[i - 1, j] L[i, j - 1]

    let mutable index = L[m, n]

    let lcs = Array.create index ' '

    let mutable i = m
    let mutable j = n

    while i > 0 && j > 0 do
        if array1[i - 1] = array2[j - 1] then
            lcs[index - 1] <- array1[i - 1]
            i <- i - 1
            j <- j - 1
            index <- index - 1
        elif L[i - 1, j] >= L[i, j - 1] then
            i <- i - 1
        else
            j <- j - 1
            lcs[index - 1] <- array2[j]

    new string(lcs)

let decodeDirective (text1: string) (text2: string) : string =
    let directive = longestCommonSubSequence text1 text2
    if directive.Length > 0 then directive
    else "-"

//Reading input and calling Function
[<EntryPoint>]
let main _ =
    Console.WriteLine("Enter the number of words in first text:")
    let n = Int32.Parse(Console.ReadLine())

    printfn "Enter the words for the first text:"
    let text1 = [ for _ in 1..n -> Console.ReadLine() ] |> String.concat " "

    Console.WriteLine("Enter the number of words in second text:")
    let m = Int32.Parse(Console.ReadLine())

    printfn "Enter the words for the second text:"
    let text2 = [ for _ in 1..m -> Console.ReadLine() ] |> String.concat " "


    let removeConsecutiveDuplicates (input: string): string =
        Seq.pairwise input
        |> Seq.fold (fun acc (a, b) -> if a <> b then acc + string(b) else acc) (string(input.[0]))

    let directive = decodeDirective text1 text2
    let directiveNoDuplicates = removeConsecutiveDuplicates directive

    Console.WriteLine($"Decoded Directive is : {directiveNoDuplicates}")

    0 // return an integer exit code