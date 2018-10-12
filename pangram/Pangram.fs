module Pangram

let isPangram (input: string): bool =
    let alphabet = ['a'..'z']
    let lowerInput = input.ToLower()
    Seq.forall (fun letter -> lowerInput.IndexOfAny [|letter|] >= 0) alphabet