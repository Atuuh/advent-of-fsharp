module AOC.Tests.Year2023.Day14

open AOC.Year2023.Day14
open NUnit.Framework

[<Test>]
let ``Should sort round rocks left of empty spots`` () =
    let row = [ Empty; Empty; Round ]
    let expected = [ Round; Empty; Empty ]
    let actual = tiltColumn row
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Should not sort round rocks past cube rocks`` () =
    let row = [ Empty; Cube; Round ]
    let expected = [ Empty; Cube; Round ]
    let actual = tiltColumn row
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Tilt should roll round rocks north`` () =
    let items =
        AOC.List.transpose [ [ Empty; Cube; Round ]; [ Round; Empty; Round ]; [ Cube; Round; Empty ] ]

    let expected =
        AOC.List.transpose [ [ Round; Cube; Round ]; [ Empty; Round; Round ]; [ Cube; Empty; Empty ] ]

    let actual = tilt items
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Rotate should rotate items correctly`` () =
    let items =
        AOC.List.transpose [ [ Empty; Cube; Round ]; [ Round; Empty; Round ]; [ Cube; Round; Empty ] ]

    let expected =
        AOC.List.transpose [ [ Cube; Round; Empty ]; [ Round; Empty; Cube ]; [ Empty; Round; Round ] ]

    let actual = rotate items
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Tilt rotate tilt should tilt items north then west`` () =
    let items =
        AOC.List.transpose [ [ Empty; Cube; Round ]; [ Round; Empty; Round ]; [ Cube; Round; Empty ] ]

    let expected =
        AOC.List.transpose [ [ Round; Cube; Round ]; [ Round; Round; Empty ]; [ Cube; Empty; Empty ] ]

    let actual = items |> tilt |> rotate |> tilt |> rotate |> rotate |> rotate
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Spin should rotate items correctly`` () =
    let items =
        AOC.List.transpose [ [ Empty; Cube; Round ]; [ Round; Empty; Round ]; [ Cube; Round; Empty ] ]

    let expected =
        AOC.List.transpose [ [ Round; Cube; Empty ]; [ Empty; Empty; Round ]; [ Cube; Round; Round ] ]

    let actual = spin items
    Assert.AreEqual(expected, actual)
