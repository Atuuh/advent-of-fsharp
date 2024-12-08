module AOC.Point

type Point = { X: int; Y: int }

let add (first: Point) (second: Point) =
    { X = first.X + second.X
      Y = first.Y + second.Y }

let subtract (first: Point) (second: Point) =
    { X = first.X - second.X
      Y = first.Y - second.Y }

let mult (point) amount =
    { X = point.X * amount
      Y = point.Y * amount }

let pointEqual a b = a.X = b.X && a.Y = b.Y
