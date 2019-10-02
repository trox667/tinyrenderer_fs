module Model

open MathNet.Numerics.LinearAlgebra

type FaceIndex =
    { iV : int
      iT : int
      iN : int }

type ObjItem =
    | Vertex of v : Vector<double>
    | Normal of n : Vector<double>
    | Face of f : FaceIndex []
    | Unknown

type Model =
    { vertices : List<Vector<double>>
      normals : List<Vector<double>>
      faces : List<FaceIndex []> }

let readLines filePath =
    filePath
    |> System.IO.File.ReadLines
    |> Seq.toList

let isEmpty (s : string) = s.Length = 0

let filterMap toObjType list =
    list
    |> List.map toObjType
    |> List.choose id

let toVertex =
    function
    | Vertex(v) -> Some(v)
    | _ -> None

let toNormal =
    function
    | Normal(n) -> Some(n)
    | _ -> None

let toFace =
    function
    | Face(f) -> Some(f)
    | _ -> None

let filterVertex = filterMap toVertex

let filterNormal = filterMap toNormal

let filterFace = filterMap toFace

let parseDoubles (line : string) =
    line.Split([| ' ' |]).[1..]
    |> Array.filter (not << isEmpty)
    |> Array.map double
    |> vector

let parseFace (line : string) =
    let items = line.Split('/')
    match items with
    | items when items.Length = 1 ->
        { iV = (int <| items.[0]) - 1
          iN = 0
          iT = 0 }
    | items when items.Length = 2 ->
        { iV = (int <| items.[0]) - 1
          iN = (int <| items.[1]) - 1
          iT = 0 }
    | items when items.Length > 2 ->
        { iV = (int <| items.[0]) - 1
          iN = (int <| items.[1]) - 1
          iT = (int <| items.[2]) - 1 }
    | _ ->
        { iV = 0
          iN = 0
          iT = 0 }

let parseFaces (line : string) =
    line.Split([| ' ' |]).[1..]
    |> Array.filter (not << isEmpty)
    |> Array.map parseFace

let parseModel filePath =
    let converter (line : string) =
        match line with
        | line when line.StartsWith("v ") -> parseDoubles >> Vertex
        | line when line.StartsWith("vn ") -> parseDoubles >> Normal
        | line when line.StartsWith("f ") -> parseFaces >> Face
        | _ -> (fun _ -> Unknown)

    let toObjItem line = converter line line

    let objItems =
        filePath
        |> readLines
        |> List.map toObjItem

    let vertices : List<Vector<double>> = filterVertex objItems
    let normals : List<Vector<double>> = filterNormal objItems
    let faces = filterFace objItems
    { vertices = vertices
      normals = normals
      faces = faces }
