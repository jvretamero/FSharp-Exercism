module Leap

let divisibleBy number year =
    year % number = 0

let divisibleByFour =
    divisibleBy 4

let divisibleByOneHundred =
    divisibleBy 100

let divisibleByFourHundred =
    divisibleBy 400

let leapYear (year: int): bool =
    (divisibleByFour year) && (not (divisibleByOneHundred year) || divisibleByFourHundred year)