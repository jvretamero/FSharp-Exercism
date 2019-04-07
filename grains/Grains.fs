module Grains
open System

let square (n: int): Result<uint64,string> =
    if n = 1 then
        Ok 1UL
    else if n = 2 then
        Ok 2UL
    else if n > 2 && n <= 64 then
        Ok (uint64 (Math.Pow(float 2, float (n - 1))))
    else
        Error "square must be between 1 and 64"

let total: Result<uint64,string> =
    let rec sum lst =
        match lst with
        | head :: tail ->
            match square head with
            | Ok v -> v + (sum tail)
            | _ -> 0UL
        | [] -> 0UL
    Ok (sum [1 .. 64])