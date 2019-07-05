module ComplexNumbers

open System

let create real imaginary = real, imaginary

let mul (a: float, b) (c: float, d) =
    (a * c - b * d), (b * c + a * d)

let add (a: float, b: float) (c: float, d: float) =
    (a + c), (b + d)

let sub (a: float, b: float) (c: float, d: float) =
    (a - c), (b - d)

let div (a: float, b: float) (c: float, d: float) =
    (a * c + b * d) / (c ** 2.0 + d ** 2.0), (b * c - a * d) / (c ** 2.0 + d ** 2.0)

let abs (a, b) =
    Math.Sqrt(a ** 2.0 + b ** 2.0)

let conjugate (a: float, b: float) =
    a, -b

let real (a, _) = a

let imaginary (_, b) = b

let exp (a, b) =
    Math.Exp(a) * Math.Cos(b), Math.Exp(a) * Math.Sin(b)