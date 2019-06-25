module AllYourBase

let reverseIndexes (digits: int list) =
    List.rev [0 .. digits.Length - 1]

let toDecimal (digits: int list) (desiredBase: int) =
    digits
    |> List.rev
    |> List.mapi (fun index digit -> digit * (pown desiredBase index))
    |> List.sum

// let toBase decimal desiredBase =
//     let rec fn acc dec =
//         if dec = 0 then
//             acc
//         else
//             fn ((dec % desiredBase) :: acc) (dec / desiredBase)
//     fn [] decimal

//The List.unfold seems a bit simpler
let toBase decimal desiredBase =
    decimal
    |> List.unfold (fun state ->
        if state = 0 then None
        else Some (state % desiredBase, state / desiredBase))
    |> List.rev

let rebase digits inputBase outputBase =
    if inputBase < 2 || outputBase < 2 then None
    else if List.exists (fun digit -> digit < 0 || digit >= inputBase) digits then None
    else if List.isEmpty digits then Some ([0])
    else if List.sum digits = 0 then Some ([0])
    else
        let decimal = toDecimal digits inputBase
        Some (toBase decimal outputBase)