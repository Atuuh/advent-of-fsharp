module AOC.Year2023.Day3

open AOC
open System.Text.RegularExpressions
open System

type Coordinate = { X: int; Y: int }

type Symbol =
    { Character: string
      Coordinate: Coordinate }

type Part =
    { PartNumber: int
      Coordinates: Coordinate list }

type PotentialPart = Part

let partNumber { PartNumber = partNumber } = partNumber
let coordinate { Coordinate = coordinate } = coordinate
let coordinates { Coordinates = coordinates } = coordinates

let potentialPart partNumber x y length : PotentialPart =
    let coords = [ x .. x + length ] |> List.map (fun _x -> { X = _x; Y = y })

    { PartNumber = partNumber
      Coordinates = coords }

let parseInput inputString : PotentialPart list * Symbol list =
    let lines = String.split '\n' inputString

    let getPotentialParts y line =
        let matches = Regex.Matches(line, "(\d+)")

        Seq.map (fun (m: Match) -> potentialPart (int m.Value) m.Index y (String.length m.Value - 1)) matches
        |> Seq.toList

    let getSymbols y line : Symbol list =
        let matches = Regex.Matches(line, "[^0-9\.]")

        Seq.map
            (fun (m: Match) ->
                { Character = m.Value
                  Coordinate = { X = m.Index; Y = y } })
            matches
        |> Seq.toList

    let potentialParts = List.mapi getPotentialParts lines |> List.flat
    let symbols = List.mapi getSymbols lines |> List.flat

    potentialParts, symbols

let isAdjacent { X = ax; Y = ay } { X = bx; Y = by } =
    ax - bx |> Math.Abs <= 1 && ay - by |> Math.Abs <= 1

let isPart (symbols: Symbol list) (potenialPart: PotentialPart) =
    List.exists
        (fun symbol -> List.exists (fun coord -> isAdjacent coord (coordinate symbol)) (coordinates potenialPart))
        symbols

let getParts potentialParts symbols : Part list =
    List.filter (isPart symbols) potentialParts

let getPartSum parts = List.sumBy partNumber parts

type Gear =
    { Coordinate: Coordinate
      Parts: Part * Part }

let gearRatio ({ Parts = a, b }: Gear) = partNumber a * partNumber b

let isAdjacentToPart ({ Coordinate = coord }: Symbol) { Coordinates = partCoords } =
    List.exists (isAdjacent coord) partCoords

let getGears parts symbols : Gear list =
    List.filter (fun { Character = char } -> char = "*") symbols
    |> List.fold
        (fun gears symbol ->
            let adjacentParts = List.filter (isAdjacentToPart symbol) parts

            match List.length adjacentParts with
            | 2 ->
                { Coordinate = coordinate symbol
                  Parts = (adjacentParts[0], adjacentParts[1]) }
                :: gears
            | _ -> gears)
        []

let private partA input =
    let potentialParts, symbols = parseInput input
    let parts = getParts potentialParts symbols
    let result = getPartSum parts
    printfn $"Part sum: {result}"

let private partB input =
    let potentialParts, symbols = parseInput input
    let parts = getParts potentialParts symbols
    let gears = getGears parts symbols
    let result = List.sumBy gearRatio gears
    printfn $"Part sum: {result}"

let solution: Types.Solution = { partA = partA; partB = partB }
