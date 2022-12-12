// HOUSEKEEPING/COMMON
let linesExample = System.IO.File.ReadLines("day11/example.txt")   |> Seq.toList;;
let linesProblem1 = System.IO.File.ReadLines("day11/problem1.txt")   |> Seq.toList;;
let linesProblem2 = System.IO.File.ReadLines("day11/problem2.txt")   |> Seq.toList;;

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

let parseMixedStringToInt (s:string) =
  try
    s.ToCharArray()|>Array.takeWhile(fun x->System.Char.IsNumber(x))|>System.String.Concat |> int
  with _->0
let parseMixedStringToBigInt (s:string) =
  try
    s.ToCharArray()|>Array.takeWhile(fun x->System.Char.IsNumber(x))|>System.String.Concat |> bigint.Parse
  with _->0



let monkeyList=linesExample|>splitBy(fun x->x.Length=0)|>Seq.toList|>List.map(fun x->x|>Seq.toList|>List.filter(fun x->x.Length>0))

let getMonkeyNumber (s:string) = 
   (s.Split([|' '|])[1]) |>parseMixedStringToInt
// getMonkeyNumber monkeyList[0][0]
let getStartingItems (s:string) = (s.Split([|' '|])|>Array.filter(fun x->x.Length<>0)|>List.ofArray)[2..]|>List.map(fun x->parseMixedStringToBigInt x)
//getStartingItems monkeyList[0][1]

let getMonkeyOperator (s:string)=s.Split([|' '|])[6]
//getMonkeyOperator (monkeyList[0][2])
let getMonkeyOperand (s:string)=s.Split([|' '|])[7]|>int
//getMonkeyOperand (monkeyList[0][2])
type Operators =
  |ADD of int 
  |SUBTRACT of int 
  |MULTIPLY of int 
  |DIVIDE of int 
  |SQUARE


let processMonkeyOperation (s:string) =
  let operator=getMonkeyOperator s 
  //let operand = getMonkeyOperand s
  let operandChunk = s.Split([|' '|])[7]
  if operandChunk="old" 
    then SQUARE 
    else 
      let operand = getMonkeyOperand s
      match operator with 
        | "+"->ADD(operand)
        | "-"->SUBTRACT(operand)
        | "*"->MULTIPLY(operand)
        | "/"->DIVIDE(operand)
        |_->ADD(operand) 
//processMonkeyOperation (monkeyList[0][2])


//(monkeyList[0][3]).Split([|' '|])|>List.ofArray|>List.last|>int;;
let getTestOperand (s:string) =
  s.Split([|' '|])|>List.ofArray|>List.last|>int
//getTestOperand (monkeyList[0][3])

let getTestOperator (s:string):string =
  let endOfString=s.Split([|' '|])[3..]
  let len=endOfString.Length
  let temp=endOfString[0..len-2]
  System.String.Join(' ',temp)

type TESTS = 
  |DIVISIBLEBY of int

let processTestOperation (s:string) = 
  let operator=getTestOperator s
  let operand=getTestOperand s 
  match operator with 
    |"divisible by"->DIVISIBLEBY(operand)
    |_->DIVISIBLEBY(operand)  
let getDivisibleBy (DIVISIBLEBY t) = bigint t

let processTrueThrowToMonkey (s:string) =
  s.Split([|' '|])|>Array.last|>int 
//processTrueThrowToMonkey (monkeyList[0][4])
let processFalseThrowToMonkey (s:string) =
  s.Split([|' '|])|>Array.last|>int 
//processTrueThrowToMonkey (monkeyList[0][5])

type Monkey =
  {
    Number:int 
    Items:bigint list 
    Operation:Operators
    Test:TESTS 
    IfTrueThrowToMonkeyNumber:int 
    IfFalseThrowToMonkeyNumber:int
  }

let processMonkey (desc:string list) =
  let monkeyNum=getMonkeyNumber desc[0]
  let monkeyItems =getStartingItems desc[1]
  let monkeyOperation=processMonkeyOperation desc[2]
  let monkeyTest=processTestOperation desc[3]
  let trueThrowToMonkey=processTrueThrowToMonkey desc[4]
  let falseThroToMonkey = processFalseThrowToMonkey desc[5]
  {Number=monkeyNum;Items=monkeyItems;Operation=monkeyOperation;Test=monkeyTest;IfTrueThrowToMonkeyNumber=trueThrowToMonkey;IfFalseThrowToMonkeyNumber=falseThroToMonkey}
  //(monkeyNum, monkeyItems, monkeyOperation,monkeyTest,trueThrowToMonkey,falseThroToMonkey)

let newWorryLevel (oldWorryLevel:bigint) (op:Operators) (test:TESTS) =
  match op with 
    |ADD(x)->(oldWorryLevel+ bigint x ) % getDivisibleBy(test)
    |SUBTRACT(x)->(oldWorryLevel- bigint  x )  % getDivisibleBy(test)
    |MULTIPLY(x)->oldWorryLevel* bigint x 
    |DIVIDE(x)->oldWorryLevel/(bigint x) 
    |SQUARE->oldWorryLevel*oldWorryLevel

//processMonkey (monkeyList[0])
let program=monkeyList|>List.map(fun x->processMonkey x)

let addItemToMonkey (monkeyNum:int) (item:bigint) (monkeys:list<Monkey> )=
  monkeys
  |> List.map(fun myMonkey->
      if myMonkey.Number=monkeyNum
        then
          let newItems=(List.append myMonkey.Items [item])
          //printfn "Added item %A to monkey number %A " item monkeyNum
          {myMonkey with Items=newItems}
        else myMonkey
    )
let replaceMonkey(myMonkey:Monkey) (monkeys:List<Monkey>) =
  //printfn "replace monkey %A" myMonkey
  monkeys|>List.map(fun x->
    if x.Number=myMonkey.Number then myMonkey else x
    )
let operateMonkey (monkeyNumber:int) (monkeys:List<Monkey>)=
  let monkeyToRun =monkeys[monkeyNumber]
  let newWorryLevel=monkeyToRun.Items|>List.map(fun x->newWorryLevel x monkeyToRun.Operation monkeyToRun.Test)
  //let boredMonkeyWorryLevel =newWorryLevel|>List.map(fun x->x/3)
  let boredMonkeyWorryLevel =newWorryLevel|>List.map(fun x->x)
  let newItems=boredMonkeyWorryLevel
  let newMonkeys =
    newItems 
    |> List.fold(fun (acc:List<Monkey>) (y:bigint)->
      let testItem = ((y)%getDivisibleBy monkeyToRun.Test)=bigint 0
      if testItem 
        then addItemToMonkey monkeyToRun.IfTrueThrowToMonkeyNumber y acc
        else addItemToMonkey monkeyToRun.IfFalseThrowToMonkeyNumber y acc
      ) monkeys

  let poo = newItems|>List.map(fun x->(x%getDivisibleBy monkeyToRun.Test)=bigint 0)
  //printfn "Tests status %A" poo
  let emptyMonkey= {monkeyToRun with Items=[]}
  let ret = newMonkeys |> replaceMonkey emptyMonkey
  (ret,monkeyToRun.Items.Length)

let round (monkeys:List<Monkey>) (activityList:int list) =
  let numMonkeys=monkeys.Length
  let processedRound=  
    [0..numMonkeys-1]|>List.fold(fun acc x->
      let startingMonkeyList = fst acc 
      let (startingActivityList:int list)= snd acc
      let oper:(List<Monkey>*int)=operateMonkey x startingMonkeyList
      let newMonkeys = fst oper 
      let numberOfItemsMonkeyTouched = snd oper 
      //printfn "number of items monkey %A touched %A" x numberOfItemsMonkeyTouched
      let runningTotalActivityList =
        if startingActivityList.Length=0
          then [numberOfItemsMonkeyTouched]
          else List.append  startingActivityList [numberOfItemsMonkeyTouched]
          // List.zip newActivityList startingActivityList |> List.map (fun (a,b)->a+b)
      ( newMonkeys,runningTotalActivityList) ) (monkeys,[])
  let newAcitivityTotals =
    if activityList.Length=0 
      then snd processedRound
      else List.zip activityList (snd processedRound)|>List.map(fun (a,b)->a+b)
  (fst processedRound, newAcitivityTotals)

let runProg n = 
  [0..n-1] 
  |> List.fold(fun acc x->
    let (oldRunningActivitiesList:int list) = snd acc
    //let newLengthOfStuffToInspect= (fst acc)|>List.map(fun x->x.Items.Length)
    ////printfn "OLD LEN %A NEW LEN %A" oldLengthOfStuffToInspec newLengthOfStuffToInspect
    let newAcc=round (fst acc) []
    let newRunningListActivities=snd newAcc
    let updatedRunningListActivities =
      if oldRunningActivitiesList.Length=0
        then newRunningListActivities
        else
          List.zip oldRunningActivitiesList newRunningListActivities|>List.map(fun (a,b)->a+b)
    (fst newAcc,updatedRunningListActivities)
    ) (program,[])

let foo=runProg 10000
let bar=(snd foo)|>List.sort|>List.rev

printfn "bar %A " bar

// PART 2
// EVERY NUMBER CAN BE EXPRESSED AS A QUAD: a base, a base to a certain power, a base times a mulitplier, and a remainder
//(a,b,c,d) a^b +a*c +d
// using this notation (and the binomial/trinomial theorm) you can avoid bigints and still do all the elf work

//
