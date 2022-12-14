// HOUSEKEEPING/COMMON
let linesExample = System.IO.File.ReadLines("day14/example.txt")   |> Seq.toList;;
let linesProblem1 = System.IO.File.ReadLines("day14/problem1.txt")   |> Seq.toList;;
let linesProblem2 = System.IO.File.ReadLines("day14/problem2.txt")   |> Seq.toList;;

let splitBy f input =
  let i = ref 0
  input
  |> Seq.groupBy (fun x ->
    //if f x then incr i
    if f x then (i.Value<-i.Value+1)
    i.Value)
  |> Seq.map snd

let stringListToCharArray (lines:string list)=lines|>List.filter(fun x->x.Length>0)|>List.map(fun x->x.ToCharArray()|>List.ofArray)|>array2D;
let getNeighbor<'a> (arr:'a [,]) x y :'a option  =
  try Some arr[x,y] with _->None
let getNeighborAndCoordinate<'a> (arr:'a [,]) x y :('a*(int*int)) option  =
  try Some (arr[x,y],(x,y)) with _->None
let getNeighbors<'a> (myWorld:'a [,]) (myPoint:int*int) (neighborsToGet:(int*int) list):'a list =
  let rootX,rootY = myPoint
  neighborsToGet|>List.map(fun x->
    let newX = rootX + fst x
    let newY = rootY + snd x
    getNeighbor<'a> myWorld newX  newY
  ) |>List.choose id
let getNeighborsAndCoordinates<'a> (myWorld:'a [,]) (myPoint:int*int) (neighborsToGet:(int*int) list):('a*(int*int)) list =
  let rootX,rootY = myPoint
  neighborsToGet|>List.map(fun x->
    let newX = rootX + fst x
    let newY = rootY + snd x
    getNeighborAndCoordinate<'a> myWorld newX  newY
  ) |>List.choose id

let getPointsSurroundingAPoint<'a> (myWorld:'a [,]) (myPoint:int*int) =
  getNeighbors<'a> myWorld myPoint [(-1,-1);(-1,0);(-1,1);(0,-1);(0,1);(1,-1);(0,1);(1,1)]
let getPointsAndCoordinatesSurroundingAPoint<'a> (myWorld:'a [,]) (myPoint:int*int) =
  getNeighborsAndCoordinates<'a> myWorld myPoint [(-1,-1);(-1,0);(-1,1);(0,-1);(0,1);(1,-1);(0,1);(1,1)]
let getRowColumnPointsAndCoordinatesSurroundingAPoint<'a> (myWorld:'a [,]) (myPoint:int*int) =
  getNeighborsAndCoordinates<'a> myWorld myPoint [(-1,0);(0,-1);(0,1);(1,0)]


let find2D needle (arr: 'a [,]) = 
    let rec go x y =
          if   y >= arr.GetLength 1 then None
          elif x >= arr.GetLength 0 then go 0 (y+1)
          elif arr.[x,y] = needle   then Some (x,y)
          else go (x+1) y
    go 0 0



let parseIntoPaths (linesInput:string list) = 
  linesInput 
  |> List.map(fun x->
    x.Split("->")
      |>List.ofArray 
      |> (List.windowed 2)
      |>List.map(fun y->
        y|>List.map(fun y->
          let tupeUp=y.Split([|','|])
          (int tupeUp[0], int tupeUp[1])
        )
      )
    )
  |>List.map(fun x->x|>List.map(fun y->(y[0],y[1])))

let example1Paths=parseIntoPaths linesExample
let problem1Paths=parseIntoPaths linesProblem1





