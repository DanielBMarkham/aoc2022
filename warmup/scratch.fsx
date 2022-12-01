// EAV or EAV/CR
// Decided that Type Instance Attribute Value datetime was all that was required
// This is at heart a logging problem
let getRecordsFrom fileName = System.IO.File.ReadLines(fileName)   |> Seq.toList |>List.map(fun x->x.Split [|'\t'|]) |> List.filter(fun x->x.Length>=5);;
let allRecords = getRecordsFrom "warmup/example.tsv";;

// Composable Lenses On The Data. Simple lenses first, then composed lenses
let allTypes (incomingRecords:string[] list) : string[] list = incomingRecords|>List.filter(fun x->x[0].Length>0)
let allEntities (incomingRecords:string[] list) : string[] list = incomingRecords|>List.filter(fun x->x[1].Length>0)
let allAttributes (incomingRecords:string[] list) : string[] list = incomingRecords|>List.filter(fun x->x[2].Length>0)
let allValues (incomingRecords:string[] list) : string[] list = incomingRecords|>List.filter(fun x->x[3].Length>0)
let allEventTimes (incomingRecords:string[] list) : string[] list = 
  incomingRecords|>List.filter(fun x->
    (x[4].Length>0) && (fst (System.DateTime.TryParse(x[4])) =true)
    )
let eventsBeforeADate (dt:System.DateTime) (incomingRecords:string[] list):string[] list=
  incomingRecords|>allEventTimes|>List.filter(fun x->
    try System.DateTime.Parse(x[4])<dt with _->false)
let eventsAfterADate (dt:System.DateTime) (incomingRecords:string[] list):string[] list=
  incomingRecords|>allEventTimes|>List.filter(fun x->
    try System.DateTime.Parse(x[4])>dt with _->false)
let eventsBetweenDates (dtFrom:System.DateTime) (dtTo:System.DateTime) (incomingRecords:string[] list):string[] list=
  incomingRecords|>allEventTimes|>eventsAfterADate dtFrom|>eventsBeforeADate dtTo
let typesMatching (sMatchingPattern:string) (incomingRecords:string[] list): string[] list=
  let matchRegex=System.Text.RegularExpressions.Regex(sMatchingPattern)
  incomingRecords|>allTypes|>List.filter(fun x->matchRegex.IsMatch x[0])
let entitiesMatching (sMatchingPattern:string) (incomingRecords:string[] list): string[] list=
  let matchRegex=System.Text.RegularExpressions.Regex(sMatchingPattern)
  incomingRecords|>allEntities|>List.filter(fun x->matchRegex.IsMatch x[1])
let attributesMatching (sMatchingPattern:string) (incomingRecords:string[] list): string[] list=
  let matchRegex=System.Text.RegularExpressions.Regex(sMatchingPattern)
  incomingRecords|>allAttributes|>List.filter(fun x->matchRegex.IsMatch x[2])
let valuesMatching (sMatchingPattern:string) (incomingRecords:string[] list): string[] list=
  let matchRegex=System.Text.RegularExpressions.Regex(sMatchingPattern)
  incomingRecords|>allValues|>List.filter(fun x->matchRegex.IsMatch x[3])
let eventTimesMatching (sMatchingPattern:string) (incomingRecords:string[] list): string[] list=
  let matchRegex=System.Text.RegularExpressions.Regex(sMatchingPattern)
  incomingRecords|>allEventTimes|>List.filter(fun x->matchRegex.IsMatch x[4])
let typeFirstChanged(incomingRecords:string[] list):string[]  =
  incomingRecords |> allTypes |> allEventTimes |> List.last
let typeLastChanged(incomingRecords:string[] list):string[]  =
  incomingRecords |> allTypes |> allEventTimes |> List.head
let entityFirstChanged(incomingRecords:string[] list):string[]  =
  incomingRecords |> allEntities |> allEventTimes |> List.last
let entityLastChanged(incomingRecords:string[] list):string[]  =
  incomingRecords |> allEntities |> allEventTimes |> List.head
let attributeFirstChanged (incomingRecords:string[] list):string[]  =
  incomingRecords |> allAttributes |> allEventTimes |> List.last
let attributeLastChanged (incomingRecords:string[] list):string[]  =
  incomingRecords |> allAttributes |> allEventTimes |> List.head
let valueFirstChanged(incomingRecords:string[] list):string[]  =
  incomingRecords |> allValues |> allEventTimes |> List.last
let valueLastChanged(incomingRecords:string[] list):string[]  =
  incomingRecords |> allValues |> allEventTimes |> List.head

type dateChunkSizes = 
  |Minute  |Hour  |Day  |Week   |Month  |Year   |CustomNumberOfMinutes of int
let reduceTimeSpanToChunkNumber (beginDateTime:System.DateTime) (chunkSize:dateChunkSizes) (dateTimeToIndex:System.DateTime) =
  let timePassedSinceStart =dateTimeToIndex.Subtract beginDateTime
  match chunkSize with
    |Minute->timePassedSinceStart.Minutes
    |Hour->timePassedSinceStart.Hours
    |Day->timePassedSinceStart.Days
    |Week->dateTimeToIndex.Day/7
    |Month->dateTimeToIndex.Year + dateTimeToIndex.Month
    |Year->dateTimeToIndex.Year
    |CustomNumberOfMinutes minuteChunkSize->
      int timePassedSinceStart.TotalMinutes / minuteChunkSize
let groupByTimePeriod (chunkSizes:dateChunkSizes) (incomingRecords:string[] list) =
  let earliestRecord = (incomingRecords|>valueFirstChanged)[4]
  let earliestTime=System.DateTime.Parse(earliestRecord)
  let recordsSortedByDate=incomingRecords|>allEventTimes|>List.sortBy(fun x->(System.DateTime.Parse(x[4])))
  recordsSortedByDate|>allEventTimes|>List.map(fun x->
    ((reduceTimeSpanToChunkNumber earliestTime chunkSizes (System.DateTime.Parse(x[4])),x))
    )
// For later. Might add some nice text output options
type PrettyPrintOptions = |ByType=0|ByInstance=1|ByAttribute=2|ByValue=3
let prettyPrint (opt:PrettyPrintOptions) (incomingRecords:string[] list) =
  incomingRecords|>List.sortBy(fun x->(x[int opt]))
    |>List.iter(fun x->printf "%A" x)
  ()


// SAMPLE DATA CREATION TOOLS

let pickRandomListItem (arr:List<'a>) =
  let r=new System.Random()
  let choice=r.Next(arr.Length)
  arr.Item(choice)
let pickRandomLightState()=pickRandomListItem(["Light Off";"Light On"])
let pickRandomLight4()=pickRandomListItem(["Simpson's House"; "Simpson's Barn"; "Home Base"; "Happy Gorilla Pub"; "Joe's Hospital"; "City Hall"])
let pickADateTimeBetweenTwoDateTimes() (sDateFrom:string) (sDateTo:string):System.DateTime =
  try
    let r=new System.Random()
    let dtFrom=System.DateTime.Parse(sDateFrom)
    let dtTo=System.DateTime.Parse(sDateTo)
    let range=int (dtTo-dtFrom).TotalMinutes
    dtFrom.AddMinutes(r.Next(range))
  with _->System.DateTime.MinValue
let getRandomLightRecord4() sFrom sTo =
  pickRandomLight4() + "\t" + pickRandomLightState() + "\t" + (pickADateTimeBetweenTwoDateTimes() sFrom sTo).ToString("g")

let randomizedLogProblem4 sFrom sTo =Seq.initInfinite(fun index->getRandomLightRecord4() sFrom sTo)

let differentInstances5=["Simpson's House"; "Simpson's Barn"; "Home Base"; "Happy Gorilla Pub"; "Joe's Hospital"; "City Hall"; "Old Beaver Dam"; "Main Street"; "Elm Street Railroad Crossing"; "Oscar Jones Home"; "Oscar Jones Barn"; "Oscar Jones Front Yard"; "Bellweather Inn"; "SnagglePus Hotel";"Remants Of The Day Consignment Shop"; "Moe's Mowers"; "Grinch's Grocery"; "City Jail";"Horace Horse";"Plum Ferry Service"]
let pickRandomInstance5()=pickRandomListItem(differentInstances5)
let differentEmployees5=["Joe Bob"; "Tom Fool" ;"Billy Bob"; "Roberta"; "Billy Sue"; "Odus"; "Miss Anthrope"; "Bigus Dickus"; "Sosue Mi"; "Nachos Normaldata"; "Hepmi"; "Gno Problem"; "Jeeves Louise"; "Orca"]
let pickRandomEmployee5()=pickRandomListItem(differentEmployees5)
let differentTypes5=["Light Bulb"; "Electric Banana"]
let pickRandomType5()=pickRandomListItem(differentTypes5)
let differentValues5=["On"; "Off"]
let pickRandomValue5()=pickRandomListItem(differentValues5)
let pickRandomRecord5() sFrom sTo =
  pickRandomType5() + "\t" + pickRandomInstance5() + "\t" + "State" + "\t" + pickRandomValue5() + "\t" + (pickADateTimeBetweenTwoDateTimes() sFrom sTo).ToString("g")
let randomizedLogProblem5 sFrom sTo =Seq.initInfinite(fun index->pickRandomRecord5() sFrom sTo)

//System.IO.File.WriteAllLines("warmup/example4.tsv", foo |> Seq.take 300)

// Stuff for essay
type CardSuit = |Hearts|Diamonds|Spades|Clubs 
let CardSuits =[Hearts;Diamonds;Spades;Clubs]
type CardRank = |Ace|Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|Jack|Queen|King
let CardRanks= [Ace;Two;Three;Four;Five;Six;Seven;Eight;Nine;Ten;Jack;Queen;King]
type PlayingCard = {Suit:CardSuit; Rank:CardRank}
let unicodeCards:(string*CardSuit*CardRank) list =
  [
  ("\U0001F0A1", Clubs, Ace);
  ("\U0001F0A2", Clubs, Two);
  ("\U0001F0A3", Clubs, Three);
  ("\U0001F0A4", Clubs, Four);
  ("\U0001F0A5", Clubs, Five);
  ("\U0001F0A6", Clubs, Six);
  ("\U0001F0A7", Clubs, Seven);
  ("\U0001F0A8", Clubs, Eight);
  ("\U0001F0A9", Clubs, Nine);
  ("\U0001F0AA", Clubs, Ten);
  ("\U0001F0AB", Clubs, Jack);
//  ("\U0001F0AC", Clubs, Knight);
  ("\U0001F0AD", Clubs, Queen);
  ("\U0001F0AE", Clubs, King);
//"\U0001F0A0" Back of card
  ("\U0001F0B1", Hearts, Ace);
  ("\U0001F0B2", Hearts, Two);
  ("\U0001F0B3", Hearts, Three);
  ("\U0001F0B4", Hearts, Four);
  ("\U0001F0B5", Hearts, Five);
  ("\U0001F0B6", Hearts, Six);
  ("\U0001F0B7", Hearts, Seven);
  ("\U0001F0B8", Hearts, Eight);
  ("\U0001F0B9", Hearts, Nine);
  ("\U0001F0BA", Hearts, Ten);
  ("\U0001F0BB", Hearts, Jack);
//  ("\U0001F0BC", Hearts, Knight);
  ("\U0001F0BD", Hearts, Queen);
  ("\U0001F0BE", Hearts, King);
// "\U0001F0BF", Hearts, Joker);
  ("\U0001F0C1", Diamonds, Ace);
  ("\U0001F0C2", Diamonds, Two);
  ("\U0001F0C3", Diamonds, Three);
  ("\U0001F0C4", Diamonds, Four);
  ("\U0001F0C5", Diamonds, Five);
  ("\U0001F0C6", Diamonds, Six);
  ("\U0001F0C7", Diamonds, Seven);
  ("\U0001F0C8", Diamonds, Eight);
  ("\U0001F0C9", Diamonds, Nine);
  ("\U0001F0CA", Diamonds, Ten);
  ("\U0001F0CB", Diamonds, Jack);
//  ("\U0001F0CC", Diamonds, Knight);
  ("\U0001F0CD", Diamonds, Queen);
  ("\U0001F0CE", Diamonds, King);
//"\U0001F0CF", Red, Joker);
  ("\U0001F0D1", Clubs, Ace);
  ("\U0001F0D2", Clubs, Two);
  ("\U0001F0D3", Clubs, Three);
  ("\U0001F0D4", Clubs, Four);
  ("\U0001F0D5", Clubs, Five);
  ("\U0001F0D6", Clubs, Six);
  ("\U0001F0D7", Clubs, Seven);
  ("\U0001F0D8", Clubs, Eight);
  ("\U0001F0D9", Clubs, Nine);
  ("\U0001F0DA", Clubs, Ten);
  ("\U0001F0DB", Clubs, Jack);
//  ("\U0001F0DC", Clubs, Knight);
  ("\U0001F0DD", Clubs, Queen);
  ("\U0001F0DE", Clubs, King);
//"\U0001F0DF", White, Trump);
  ]

let playingDeck: (CardSuit * CardRank) list=CardSuits|> List.collect (fun x -> CardRanks |> List.map (fun y-> x, y));;
let deal numHands handSize cards =
  let rnd=System.Random()
  let shuffledCards=cards |> List.sortBy(fun _ ->rnd.Next(1,52))
  ((shuffledCards|>List.chunkBySize handSize |> Seq.take numHands), (shuffledCards|>Seq.skip (numHands*handSize)))

let lookupCardUnicode (card:(CardSuit*CardRank)):string =
  let ret= unicodeCards|>List.tryFind(fun (char,suit,rank)->fst card=suit && snd card=rank)
  match ret with 
    | Some (a,b,c)->a 
    |_None->""
