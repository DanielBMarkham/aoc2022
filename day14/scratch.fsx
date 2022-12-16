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



type CaveCell= |Air|Rock|Sand
let playGrid (inputLines:string list) = 
  let paths=parseIntoPaths(inputLines)
  let biggestX (paths:((int*int)*(int*int)) list list ):int =
    (fst (paths|>List.concat|>List.map(fun (a,b)->[a;b])|>List.concat|>List.maxBy(fun (a,b)->a)))
  let biggestY (paths:((int*int)*(int*int)) list list ):int =
    snd (paths|>List.concat|>List.map(fun (a,b)->[a;b])|>List.concat|>List.maxBy(fun (a,b)->b))
  let maxX=biggestX paths
  let maxY=biggestY paths 
  let playingGrid=Array2D.create (maxX + 1) (maxY + 1) Air
  playingGrid
  
let pathPointsToFilledGrid (myWorld:'a [,]) (itemToAdd:'a) (endPoints:((int*int)*(int*int))) =
  let beginPoint = fst endPoints 
  let endPoint = snd endPoints
  let flipPairToMinfirst (a,b)= if b<a then (b,a) else (a,b)
  let rangeToMap =
    match (fst beginPoint = fst endPoint), (snd beginPoint = snd endPoint) with
      | false,false -> []
      | true, true->[]
      | false, true->
        let orderPair=flipPairToMinfirst(fst beginPoint, fst endPoint)
        [fst orderPair..snd orderPair] |>List.map(fun x-> (x,snd endPoint))
      | true,false->
        let orderPair=flipPairToMinfirst(snd beginPoint, snd  endPoint)
        [fst orderPair..snd orderPair] |>List.map(fun x-> (fst endPoint, x))
  let addPoint i j (theWorld) (thingToAdd:'a)=
    theWorld|>Array2D.mapi(fun x y z->if (i=x) && (j=y) then thingToAdd else z)
  printfn "range to map %A" rangeToMap
  rangeToMap|>List.fold(fun acc x->(addPoint (fst x) (snd x) acc itemToAdd)) myWorld


let foo= Array2D.init 7 7 (fun i j->Air)



#r "System.Windows.Forms"