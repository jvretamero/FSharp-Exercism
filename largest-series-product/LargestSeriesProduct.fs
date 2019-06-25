module LargestSeriesProduct

open System

let largestProduct (input: string) seriesLength : int option =
    if Seq.exists (Char.IsDigit >> not) input then
        None
    else
        match input.Length with
        | len when len < seriesLength -> None
        | 0 when seriesLength = 0 -> Some 1
        | _ when seriesLength = 0 -> Some 1
        | _ when seriesLength < 0 -> None
        | _ ->
                input
                |> Seq.map (string >> int)
                |> Seq.windowed seriesLength
                |> Seq.map (Seq.reduce ( * ))
                |> Seq.max
                |> Some