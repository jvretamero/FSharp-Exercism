module Isogram

open System

let isIsogram (str: string) =
    str.ToCharArray ()
    |> Seq.map (Char.ToLower)
    |> Seq.filter (Char.IsLetter)
    |> Seq.countBy (id)
    |> Seq.exists (fun (_, count) -> count > 1)
    |> not