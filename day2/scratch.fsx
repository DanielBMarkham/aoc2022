
let linesExample = System.IO.File.ReadLines("day2/example.txt")   |> Seq.toList;;
let linesProblem1 = System.IO.File.ReadLines("day2/problem1.txt")   |> Seq.toList;;
let linesProblem2 = System.IO.File.ReadLines("day2/problem1.txt")   |> Seq.toList;;


type GameToken = |Rock|Paper|Scissors
type GameOutcome = |Win|Lose|Draw
let firstColMatch col:GameToken option =
  match col with
    | 'A'->Some Rock
    | 'B'->Some Paper
    | 'C'->Some Scissors
    |_->None 
let secondColMatch col:GameToken option =
  match col with 
    | 'X'->Some Rock 
    | 'Y'->Some Paper
    | 'Z'->Some Scissors
    |_->None 
let problem2SecondColMatch col:GameOutcome option =
  match col with 
    | 'X'->Some Lose
    | 'Y'->Some Draw
    | 'Z'->Some Win
    |_->None 

let procRow (str:string):(GameToken* GameToken) option =
  try
    let c1= str[0]
    let c2=str[2]
    let t1=firstColMatch c1 
    let t2=secondColMatch c2 
    Some(t1.Value,t2.Value)
  with _->None
let procRowProblem2 (str:string):(GameToken* GameOutcome) option =
  try
    let c1= str[0]
    let c2=str[2]
    let t1=firstColMatch c1 
    let t2=problem2SecondColMatch c2 
    Some(t1.Value,t2.Value)
  with _->None


let ex1=linesExample|>List.map(fun x->procRow x) |> List.choose id
let ex2=linesExample|>List.map(fun x->procRowProblem2 x) |> List.choose id
let prob1=linesProblem1|>List.map(fun x->procRow x) |> List.choose id
let prob2=linesProblem2|>List.map(fun x->procRowProblem2 x) |> List.choose id
// 1 rock 2 paper 3 scissor 
let scoreGame handOne handTwo = 
  match handOne, handTwo with
    | Rock,Rock->(4,4)//false // 3 is a draw 1 for rock
    | Rock,Paper->(1,8)//false 1 for the rock 2 for the paper 6 for the winner
    | Rock, Scissors->(7,3)//true rock 1 scissors 3// rock beats scissors 6 is a win
    | Paper, Rock->(8,1)//false  // 2 paper 1 rock paper beats rock 6 is a win
    | Paper, Paper->(5,5)//false paper 2 // 3 is a draw
    | Paper, Scissors->(2,9)//true 2 for paper 3 for scissors scissors wins 6
    | Scissors, Rock->(3,7)//false 3 scissors 1 rock rock beats scissors win 6
    | Scissors, Paper->(9,2)//false  // scissors beats paper 6 is a win scissors 3 paper 2
    | Scissors, Scissors->(6,6)//false //3 is a draw 3 for scissors

let exampleGameScores = ex1 |> List.map(fun (a,b)->scoreGame a b)
let problem1GameScores = prob1 |> List.map(fun (a,b)->scoreGame a b)

let pickSecondHandGivenDesiredOutput (firstHand:GameToken) (desiredOutcome:GameOutcome)=
  match firstHand,desiredOutcome with 
    |Rock,Win->Scissors
    |Rock,Lose->Paper
    |Rock,Draw->Rock 
    |Paper,Win->Scissors
    |Paper,Lose->Rock 
    |Paper,Draw->Paper 
    |Scissors, Win->Paper
    |Scissors,Lose ->Rock
    |Scissors,Draw->Scissors

let problem2Game=prob2|>List.map(fun (x,y)->(x, pickSecondHandGivenDesiredOutput x y))
let example2=ex2|>List.map(fun (x,y)->(x, pickSecondHandGivenDesiredOutput x y))
//ex1 |> List.map(fun (x,y)->scoreGame x y)|>List.sumBy(fun x->fst x);;
//problem1GameScores|> List.map(fun (x,y)->scoreGame x y)|>List.sumBy(fun x->fst x);;