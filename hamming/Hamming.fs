module Hamming

let pairs firstStrand secondStrand =
    List.zip (Seq.toList firstStrand) (Seq.toList secondStrand)

let hamming pairs =
    List.fold
        (fun acc (leftItem, rightItem) ->
            acc + if leftItem.Equals rightItem then 0 else 1) 0 pairs

let distance (strand1: string) (strand2: string): int option =
    if strand1.Length = 0 || strand1.Length = 0 then
        Some 0
    else if strand1.Length = strand2.Length then
        Some (hamming (pairs strand1 strand2))
    else
        None