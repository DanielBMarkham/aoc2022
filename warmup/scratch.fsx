// Composable Lenses On The Data

let getRecordsFrom fileName = System.IO.File.ReadLines(fileName)   |> Seq.toList |>List.map(fun x->x.Split [|'\t'|]) |> List.filter(fun x->x.Length>=5);;
let allRecords = getRecordsFrom "warmup/example.tsv";;
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
    try
      System.DateTime.Parse(x[4])<dt
    with _->false
  )
let eventsAfterADate (dt:System.DateTime) (incomingRecords:string[] list):string[] list=
  incomingRecords|>allEventTimes|>List.filter(fun x->
    try
      System.DateTime.Parse(x[4])>dt
    with _->false
  )
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
  |Minute
  |Hour
  |Day
  |Week 
  |Month 
  |CustomNumberOfMinutes of int
let reduceTimeSpanToChunkNumber (beginDateTime:System.DateTime) (chunkSize:dateChunkSizes) (dateTimeToIndex:System.DateTime) =
  let timePassedSinceStart =dateTimeToIndex.Subtract beginDateTime
  match chunkSize with
    |Minute->timePassedSinceStart.Minutes
    |Hour->timePassedSinceStart.Hours
    |Day->timePassedSinceStart.Days
    |Week->dateTimeToIndex.Day%7
    |Month->dateTimeToIndex.Year + dateTimeToIndex.Month
    |CustomNumberOfMinutes minuteChunkSize->
      int timePassedSinceStart.TotalMinutes % minuteChunkSize
let groupByTimePeriod (chunkSizes:dateChunkSizes) (incomingRecords:string[] list) =
  let earliestRecord = (incomingRecords|>valueFirstChanged)[4]
  let earliestTime=System.DateTime.Parse(earliestRecord)
  incomingRecords|>List.map(fun x->((reduceTimeSpanToChunkNumber earliestTime chunkSizes System.DateTime.Parse(x[4])),x))
      |>List.sortBy(fun x->fst x) |>List.map(fun x->snd x)
  //incomingRecords
type PrettyPrintOptions = |ByType=0|ByInstance=1|ByAttribute=2|ByValue=3
let prettyPrint (opt:PrettyPrintOptions) (incomingRecords:string[] list) =
  incomingRecords|>List.sortBy(fun x->(x[int opt]))
    |>List.iter(fun x->printf "%A" x)
  ()

// EAV or EAV/CR
// Decided that Type Instance Attribute Value datetime was all that was required
// It's a logging question!

// Tools to make sample data

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
