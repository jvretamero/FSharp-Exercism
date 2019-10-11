module RationalNumbers

type RationalNumber = {
    Numerator: int;
    Denominator: int;
}

let rec gcd r =
    if r.Denominator = 0
    then r.Numerator
    else gcd { Numerator = r.Denominator; Denominator = r.Numerator % r.Denominator; }

let reduce r =
    let d = gcd r
    { Numerator = r.Numerator / d; Denominator = r.Denominator / d; }

let create numerator denominator =
    { Numerator = numerator; Denominator = denominator; }
    |> reduce

let add r1 r2 =
    create
        (r1.Numerator * r2.Denominator + r2.Numerator * r1.Denominator)
        (r1.Denominator  * r2.Denominator)

let sub r1 r2 =
    create
        (r1.Numerator * r2.Denominator - r2.Numerator * r1.Denominator)
        (r1.Denominator  * r2.Denominator)

let mul r1 r2 =
    create
        (r1.Numerator * r2.Numerator)
        (r1.Denominator  * r2.Denominator)

let div r1 r2 =
    create
        (r1.Numerator * r2.Denominator)
        (r1.Denominator  * r2.Numerator)

let abs r =
    create (abs r.Numerator) (abs r.Denominator)

let pow b e =
    int ((double b) ** (double e))

let exprational n r =
    create (pow r.Numerator n) (pow r.Denominator n)

let expreal r n =
    (double (pow n r.Numerator)) ** (1.0 / (double r.Denominator))