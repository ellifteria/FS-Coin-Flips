module CoinFlipAnalysis =
    let containsRunInCenter (graphemes: char seq): bool  =
        let first: char = Seq.head(graphemes)
        let last : char= graphemes |> Seq.rev |> Seq.head
        let center: char seq = graphemes |> Seq.tail |> Seq.rev |> Seq.tail

        (not (center |> Seq.exists(fun (elm: char) -> elm = first))) &&
        (not (center |> Seq.exists(fun (elm: char) -> elm = last))) &&
        ((center |> Seq.distinct |> Seq.length) = 1)

    let countRunsOfLength (graphemes: string) (len: int): int =
        (0, ["X"; "X"]  |> String.concat (graphemes)
                        |> Seq.windowed (len + 2))
        ||> Seq.fold (fun acc x ->
            if containsRunInCenter(x) then
                acc + 1
            else
                acc)

    let countAllRuns (flips: string) =
        for len = 1 to String.length(flips) do
            let count = countRunsOfLength flips len
            if count <> 0 then
                printfn "%d: %d" len count

    let generateRandomFlips (len: int): string =
        let rnd = System.Random()
        [for i in 1..len do rnd.Next(2)]
        |> Seq.map(fun x->
            match x with
            | 0 -> "H"
            | 1 -> "T")
        |> String.concat ""

    let generateNextSemiRandom (curr: string) (gamma: float): string =
        let rnd = System.Random()
        match (rnd.NextDouble() < gamma, curr) with
        | (true, _) -> curr
        | (false, "H") -> "T"
        | (false, "T") -> "H"

    let generateSemiRandomFlips (len: int) (gamma: float): string =    
        let mutable flips = [generateRandomFlips 1]
        for i = 1 to (len - 1) do
            flips <- (generateNextSemiRandom flips.Head gamma) :: flips
        flips |> List.rev
        |> List.toSeq
        |> String.concat ""

[<EntryPoint>]
let main args =
    let len = args[0] |> int
    let gamma = args[1] |> float
    printfn "generating length: %d" len
    let randomFlips = CoinFlipAnalysis.generateRandomFlips len
    printfn "random: %s" randomFlips
    CoinFlipAnalysis.countAllRuns randomFlips
    let semiRandomFlips = CoinFlipAnalysis.generateSemiRandomFlips len gamma
    printfn "semi-random: %s" semiRandomFlips
    CoinFlipAnalysis.countAllRuns semiRandomFlips
    0
