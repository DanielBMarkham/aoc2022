// HOUSEKEEPING/COMMON
let linesExample = System.IO.File.ReadLines("day10/example.txt")   |> Seq.toList;;
let linesProblem1 = System.IO.File.ReadLines("day10/problem1.txt")   |> Seq.toList;;
let linesProblem2 = System.IO.File.ReadLines("day10/problem2.txt")   |> Seq.toList;;

let splitBy f input =
  let i = ref 0
  input
  |> Seq.groupBy (fun x ->
    //if f x then incr i
    if f x then (i.Value<-i.Value+1)
    i.Value)
  |> Seq.map snd

let getNeighbor<'a> (arr:'a [,]) x y :'a option  =
  try Some arr[x,y] with _->None
let getNeighbors<'a> (myWorld:'a [,]) (myPoint:int*int) (neighborsToGet:(int*int) list):'a list =
  let rootX,rootY = myPoint
  neighborsToGet|>List.map(fun x->
    let newX = rootX + fst x
    let newY = rootY + snd x
    getNeighbor<'a> myWorld newX  newY
  ) |>List.choose id
let find2D needle (arr: 'a [,]) = 
    let rec go x y =
          if   y >= arr.GetLength 1 then None
          elif x >= arr.GetLength 0 then go 0 (y+1)
          elif arr.[x,y] = needle   then Some (x,y)
          else go (x+1) y
    go 0 0


type ElfRadioInstruction = |NOOP|ADDX of int
let getOffSetValue (ADDX x) = x

let processInputToCommands (linesInput:list<string>)=
  linesInput
  |>List.map(fun x->
    let split=x.Split([|' '|])
    match split[0] with 
      |"addx"->ADDX(int split[1])
      |_->NOOP
    )

let trivial =[
  "noop"
  ;"addx 3"
  ;"addx -5"]


type ExecutionHistory = int*int*ElfRadioInstruction //cycle number, register value, currentInstruction
let getCurrentRegisterValue (x:ExecutionHistory) = 
  let (a,b,c)=x
  b

let runProgram (linesInput:list<string>)=
  let commands=processInputToCommands linesProblem1
  commands
  |> List.fold(fun (acc:ExecutionHistory list) (x:ElfRadioInstruction)->
    let (cycleNumber,currentRegisterValue,currentInstruction) = try (acc|>List.head) with _->(0,1,NOOP)
    let newHistoryChunk:ExecutionHistory list = 
      match x with 
        |NOOP->
          let firstCycle= ExecutionHistory(cycleNumber + 1, currentRegisterValue, NOOP)
          [firstCycle]
        |ADDX(registerChange)->
          let firstCycle= ExecutionHistory(cycleNumber + 1, currentRegisterValue,x)
          let secondCycle= ExecutionHistory( (cycleNumber + 2), (currentRegisterValue + registerChange),x)
          [firstCycle;secondCycle] |> List.rev
    List.append newHistoryChunk acc
    ) [] |>List.rev

let foo=(runProgram linesExample)

let registerValueDuringExecution (lis:ExecutionHistory list) (index:int) =
  let (cycleNumber,currentRegisterValue,currentInstruction) = try lis[index] with _->(0,1,NOOP)
  match currentInstruction with 
    | NOOP->currentRegisterValue
    | ADDX(n)->
      try if lis[index-1]<>lis[index] then getCurrentRegisterValue lis[index-1] else currentRegisterValue with _-> currentRegisterValue

let instructionsWithCurrentValues =
  foo
  |>List.mapi(fun i y->
    let (a,b,c)=y
    let total= (registerValueDuringExecution foo i)
    (a,b,total)
    )

let ss =instructionsWithCurrentValues|>List.map(fun (a,b,c)->a*c)|>List.mapi(fun i x->if ((i-19)%40=0) then Some x else None) |> List.choose id


let pp n = 
  foo
  |>List.take n
  |>List.iteri(fun i y->printfn "%A       %A" y  (registerValueDuringExecution foo i) )

let prob1Answer = ss |>List.sum

let prob2Bits=instructionsWithCurrentValues|>List.chunkBySize 40|>List.map(fun x->x|>List.mapi(fun i (a,b,c)->(i>=c-1) && (i<=c+1)))

// for fun
let esc = string (char 0x1B)
let makeRed s = (esc + "[31;1m" + s + esc + "[0m")
let makeGreen s = (esc + "[32;1m" + s + esc + "[0m")
let makeBlack s = (esc + "[30;1m" + s + esc + "[0m")
let makeYellow s = (esc + "[33;1m" + s + esc + "[0m")

let bits2Terminal = 
  prob2Bits
  |>List.map(fun x->
    x|>List.map(fun y->
      if y then (makeBlack "X") else (makeYellow " ")
    ) |> System.String.Concat
  )


//bits2Terminal |> List.iter(fun x->printfn "%s" x);;