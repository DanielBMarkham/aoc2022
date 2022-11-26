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

type PrettyPrintOptions = |ByType=0|ByInstance=1|ByAttribute=2|ByValue=3
let prettyPrint (opt:PrettyPrintOptions) (incomingRecords:string[] list) =
  incomingRecords|>List.sortBy(fun x->(x[int opt]))
    |>List.iter(fun x->printf "%A" x)
  ()

// EAV or EAV/CR
// Decided that Type Instance Attribute Value datetime was all that was required
// It's a logging question!
