module CoinFlipAnalysis =
    let containsRunInCenter (graphemes: char seq): bool  =
        let first: char = Seq.head(graphemes)
        let last: char= graphemes |> Seq.rev |> Seq.head
        let center: char seq = graphemes |> Seq.tail |> Seq.rev |> Seq.tail

        (not (center |> Seq.exists(fun (elm: char) -> elm = first))) &&
        (not (center |> Seq.exists(fun (elm: char) -> elm = last))) &&
        ((center |> Seq.distinct |> Seq.length) = 1)

    let countRunsOfLength (graphemes: string) (len: int): int =
        (0, ["X"; "X"] |> String.concat graphemes
            |> Seq.windowed (len + 2))
        ||> Seq.fold (fun acc x ->
            if containsRunInCenter(x) then
                acc + 1
            else
                acc)

    let countAllRuns (flips: string) =
        let mutable gammaFlips = 0
        for len = 1 to String.length(flips) do
            let count = countRunsOfLength flips len
            gammaFlips <- gammaFlips + count
            if count <> 0 then
                printfn "%d: %d" len count
        (String.length flips) - gammaFlips + 1

    let generateRandomFlips (len: int): string =
        let rnd = System.Random()
        [for i in 1..len do rnd.Next(2)]
        |> Seq.map(fun x->
            match x with
            | 0 -> "H"
            | 1 -> "T"
            | _ -> failwith "Invalid int in flip string")
        |> String.concat ""

    let generateNextSemiRandom (curr: string) (gamma: float): string =
        let rnd = System.Random()
        match (rnd.NextDouble() < gamma, curr) with
        | (true, _) -> curr
        | (false, "H") -> "T"
        | (false, "T") -> "H"
        | _ -> failwith "Invalid character in flip string"

    let generateSemiRandomFlips (len: int) (gamma: float): string =    
        let mutable flips = [generateRandomFlips 1]
        for i = 1 to (len - 1) do
            flips <- (generateNextSemiRandom flips.Head gamma) :: flips
        flips |> List.rev
        |> List.toSeq
        |> String.concat ""

module Estimators =
    let MLE (N: int) (K: int): float =
        (K |> float) / (N |> float)

    let continuousMAP (N: int) (K: int) (aH: float) (aT: float): float =
        ((K |> float) + aH) / ((N |> float) + aH + aT)

    let discreteMAP (N: int) (K: int) (hypotheses: (float * float) seq): float =
        hypotheses |> Seq.map(fun hypothesis ->
            let (h, probH) = hypothesis
            (h, ((h ** K) * (1.0 - h) ** ((N |> float) - (K |> float))) * probH))
        |> Seq.maxBy (fun s ->
            let (a, b) = s
            b
        ) |> fst


[<EntryPoint>]
let main args =
    let len = args[0] |> int
    let gamma = args[1] |> float
    let hypotheses = seq { (args[2] |> float, args[3] |> float); (args[4] |> float, args[5] |> float)}
    let aH = args[6] |> float
    let aT = args[7] |> float

    printfn "Generating flips of length: %d" len

    printfn "\nGenerating random flips:"
    let randomFlips = CoinFlipAnalysis.generateRandomFlips len
    printfn "Random flips: %s" randomFlips
    printfn "Run counts:"
    let kRandom = CoinFlipAnalysis.countAllRuns randomFlips
    printf "Statistics:"
    printfn "MLE %f" (Estimators.MLE len kRandom)
    printfn "continuous MAP %f" (Estimators.continuousMAP len kRandom aH aT)
    printfn "discrete MAP %f" (Estimators.discreteMAP len kRandom hypotheses)

    printfn "\nGenerating semi-random flips with gamma = %0.2f:" 0.5
    let knownSemiRandomFlips = CoinFlipAnalysis.generateSemiRandomFlips len 0.5
    printfn "Semi-random flips: %s" knownSemiRandomFlips
    printfn "Run counts:"
    let kFixedSemiRandom = CoinFlipAnalysis.countAllRuns knownSemiRandomFlips
    printf "Statistics:"
    printfn "MLE %f" (Estimators.MLE len kFixedSemiRandom)
    printfn "continuous MAP %f" (Estimators.continuousMAP len kFixedSemiRandom aH aT)
    printfn "discrete MAP %f" (Estimators.discreteMAP len kFixedSemiRandom hypotheses)

    printfn "\nGenerating semi-random flips with gamma = %0.2f:" gamma
    let semiRandomFlips = CoinFlipAnalysis.generateSemiRandomFlips len gamma
    printfn "Semi-random flips: %s" semiRandomFlips
    printfn "Run counts:"
    let kSemiRandom = CoinFlipAnalysis.countAllRuns semiRandomFlips
    printf "Statistics:"
    printfn "MLE %f" (Estimators.MLE len kSemiRandom)
    printfn "continuous MAP %f" (Estimators.continuousMAP len kSemiRandom aH aT)
    printfn "discrete MAP %f" (Estimators.discreteMAP len kSemiRandom hypotheses)

    0
