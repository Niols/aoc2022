module Format = ExtFormat
module List = ExtList
module String = ExtString

module Read = ExtRead
module Write = ExtWrite

let pf = Format.printf
let epf = Format.eprintf
let fpf = Format.fprintf
let spf = Format.sprintf

let soi = string_of_int
let ios = int_of_string
let foi = float_of_int
let iof = int_of_float

let (||>) f g x = f x |> g

let curry f = fun a b -> f (a, b)
let uncurry f = fun (a, b) -> f a b

let flip f x y = f y x
