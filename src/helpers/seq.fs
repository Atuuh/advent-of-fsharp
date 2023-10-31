module AOC.Seq

let zip4 l1 l2 l3 l4 =
    Seq.zip l1 (Seq.zip3 l2 l3 l4)
    |> Seq.map (fun (x1, (x2, x3, x4)) -> x1, x2, x3, x4)
