#r "nuget: Deedle"
#r "nuget: NodaTime"

open System
open System.IO
open System.Globalization
open Deedle
open NodaTime
open NodaTime.Text

type Report =
    { Start: LocalDateTime
      End: LocalDateTime
      TotalReviews: int
      General: Map<string, int>
      Tdd: Map<string, int>
      RequirementsGathering: Map<string, int>
      Debugging: Map<string, int>
      Weeks: Map<string, int>
      Surprises: seq<string>
      Flags: seq<string> }

module Format =

    let culture = CultureInfo "en-GB"

    let date = "d/M/yyyy"

    let dateTime = $"{date} h:mmtt"

    let longDate = "d MMM yyyy"

module Column =

    let date = "Date"

    let uuid = "Review"

    let general = "General aspects about the review"

    let surprises = "New trend or surprising behaviour"

    let tdd = "TDD process"

    let requirements = "Requirements-gathering process"

    let debugging = "Debugging process"

    let week = "Week (from Review)"

    let trends column = $"Trends - {(column: string)}"

module Header =

    let frequency = "Trends frequency"

    let general = "General"

    let tdd = Column.tdd

    let debugging = Column.debugging

    let requirements = Column.requirements

    let week = "Review weeks"

    let surprises = "Surprising behaviour"

    let flags = "Devs flagged for attention (with at least 4 negative trends \
        and no notable improvement)"

module Trend =

    let cancellations = "Cancellations"

    let positive = "Notable improvement between sessions"

    let reviewTotal = "Total reviews during this period"

    let exclusions =
        [ "No-show"
          "No UUID provided"
          positive
          "UUID error" ]

    let containers =
        [ Column.tdd
          Column.requirements
          Column.debugging
          Column.general ]

    let week =
        {| Invalid = "NaN"
           Before = "Before Week 12"
           After = "After Week 12" |}

module Read =

    module private Message =

        let start = "Start date: "

        let end' = "End date: "

        let csvPath = "Reviews CSV path: "

        let cancellations =
            "Number of cancellations in the period: "

        let acceptFlags = "Y/n to accept or reject these flags:"

        let reportPath = "Target report path: "

    let private input message =
        printf "%s" message
        Console.ReadLine()

    let private parseDate schema message =
        let pattern =
            LocalDateTimePattern.Create(schema, Format.culture)

        let result = pattern.Parse message

        if result.Success then
            Some result.Value
        else
            None

    let rec private userDate message () =
        match message |> input |> parseDate Format.date with
        | Some date -> date
        | None -> userDate message ()

    let private startDate = userDate Message.start

    let private endDate = userDate Message.end'

    let private csvDate value = parseDate Format.dateTime value

    let rec private archive () =
        try
            input Message.csvPath
            |> Frame.ReadCsv
            |> Frame.rows
        with :? FileNotFoundException -> archive ()

    let private reviewInRange start end' (row: ObjectSeries<string>) =
        let date = row.GetAs<string> Column.date |> csvDate
        date >= start && date < end'

    let rec private reviews archive start end' =
        (archive: Series<int, ObjectSeries<string>>)
        |> Series.filterValues (reviewInRange start end')

    let rec cancellations () =
        let userInput = input Message.cancellations

        try
            Int32.Parse userInput
        with :? FormatException -> cancellations ()

    let acceptFlags names =
        printfn "%s" Message.acceptFlags

        let rec predicate name =
            match input $"{name}: " |> fun s -> s.ToLower() with
            | "y"
            | "yes" -> true
            | "n"
            | "no" -> false
            | _ -> predicate name

        Seq.filter predicate names

    let target () = input Message.reportPath

    let report () =
        let start = startDate ()
        let end' = endDate ()
        let archive = archive ()

        {| StartDate = start
           EndDate = end'
           Reviews = reviews archive (Some start) (Some end')
           Archive = archive |}

module Evaluate =

    let private reviewCount rows = Series.countValues rows

    let private hasTrend (trend: string) (value: string) = value.Contains trend

    let private trendFilter column trend row =
        (row: ObjectSeries<string>).GetAs<string> column
        |> hasTrend trend

    let private trend column label rows =
        let column = Column.trends column

        Series.filterValues (trendFilter column label) rows
        |> Series.countValues
        |> fun count -> label, count

    let private category label trends rows =
        Seq.map (fun t -> trend label t rows) trends

    let private isNonEmpty = String.IsNullOrWhiteSpace >> not

    let private cellIsNonEmpty (column: string) (row: ObjectSeries<string>) = row.GetAs<string> column |> isNonEmpty

    let cell column (row: ObjectSeries<string>) = row.GetAs<string> column

    let private surprisingTrends rows =
        let column = Column.trends Column.surprises

        Series.filterValues (cellIsNonEmpty column) rows
        |> Series.mapValues (cell column)
        |> Series.values

    let private countFolder counts trend =
        let count =
            match Map.tryFind trend counts with
            | None -> 1
            | Some count -> count + 1

        Map.add trend count counts

    let private trendValues (s: string) = s.Split "," |> Array.filter isNonEmpty

    let private countTrend column rows =
        (rows: Series<'a, ObjectSeries<string>>)
        |> Series.mapValues (cell <| Column.trends column)
        |> Series.values
        |> Seq.collect trendValues
        |> Seq.fold countFolder Map.empty

    let private excludeTrend trends excludedTrend = Map.remove excludedTrend trends

    let private countNegativeTrend container rows =
        let trends = countTrend container rows
        Seq.fold excludeTrend trends Trend.exclusions

    let private getUuid (row: ObjectSeries<string>) = row.GetAs<string> Column.uuid

    let private uuidFilter uuid (row: ObjectSeries<string>) = getUuid row = uuid

    let private hasPositive (s: string) = s.Contains Trend.positive

    let private lastImproved uuid rows =
        rows
        |> Series.filterValues (uuidFilter uuid)
        |> Series.lastValue
        |> (Column.trends Column.general |> cell)
        |> hasPositive

    let private hadMultipleReviews archive uuid =
        archive
        |> Series.filterValues (uuidFilter uuid)
        |> Series.countValues
        |> (<>) 1

    let private negativeTrends row container =
        countNegativeTrend container ([ row ] |> Series.ofValues)

    let private countSingleNegatives count trends = Map.count trends + count

    let private hasAtLeast4NegativeTrends (row: ObjectSeries<string>) =
        Trend.containers
        |> Seq.map (negativeTrends row)
        |> Seq.fold countSingleNegatives 0
        |> (fun count -> count >= 4)

    let private didNotImprove rows row =
        getUuid row
        |> fun name -> lastImproved name rows |> not

    let private underperformed rows row =
        hasAtLeast4NegativeTrends row
        && didNotImprove rows row

    let private devFlagFolder archive rows uuids row =
        let uuid = getUuid row

        if underperformed rows row
           && hadMultipleReviews archive uuid then
            Set.add uuid uuids
        else
            uuids

    let private flag archive acceptFlags rows =
        Series.foldValues (devFlagFolder archive rows) Set.empty rows
        |> acceptFlags

    let private weekFolder counts row =
        let week = cell Column.week row

        let add trend = countFolder counts trend

        try
            let week = int week

            if week < 12 && week > 0 then
                add Trend.week.Before
            else if week > 12 then
                add Trend.week.After
            else
                add Trend.week.Invalid
        // FormatException is thrown when the int function fails to parse
        with :? FormatException -> add Trend.week.Invalid

    let private weekCount rows =
        Series.foldValues weekFolder Map.empty rows

    let private tdd rows = countTrend Column.tdd rows

    let private general cancellations rows =
        let counts = countTrend Column.general rows
        Map.add Trend.cancellations (cancellations ()) counts

    let private requirementsGathering rows = countTrend Column.requirements rows

    let private debugging rows = countTrend Column.debugging rows

    let report archive start end' cancellations acceptFlags reviews =
        { Start = start
          End = end'
          TotalReviews = reviewCount reviews
          General = general cancellations reviews
          Tdd = tdd reviews
          RequirementsGathering = requirementsGathering reviews
          Debugging = debugging reviews
          Weeks = weekCount reviews
          Surprises = surprisingTrends reviews
          Flags = flag archive acceptFlags reviews }

module Print =

    let private title startDate endDate =
        let pattern =
            LocalDateTimePattern.Create(Format.longDate, Format.culture)

        $"Trend report for period: {pattern.Format startDate} - \
         {pattern.Format endDate}\n"

    let private frequency label count = $"{label}: {count}\n"

    let private reviewTotal total = frequency Trend.reviewTotal total

    let private category folder label values = Seq.fold folder $"{label}:\n" values

    let private table label (frequencies: Map<string, int>) =
        let folder state (key, value) = state + frequency key value

        category folder label <| Map.toList frequencies

    let private listing label entries =
        category (fun state e -> $"{state}{e}\n") label entries

    let private (.+.) report update = report + "\n" + update

    let private write target s =
        use writer = target () |> File.CreateText
        fprintfn writer "%s" s

    let report target value =
        title value.Start value.End
        .+. reviewTotal value.TotalReviews
        .+. $"{Header.frequency}:\n"
        .+. table Header.general value.General
        .+. table Header.tdd value.Tdd
        .+. table Header.requirements value.RequirementsGathering
        .+. table Header.debugging value.Debugging
        .+. table Header.week value.Weeks
        .+. listing Header.surprises value.Surprises
        .+. listing Header.flags value.Flags
        |> write target

let generateReport () =
    let data = Read.report ()

    let value =
        Evaluate.report data.Archive data.StartDate data.EndDate Read.cancellations Read.acceptFlags data.Reviews

    Print.report Read.target value

generateReport () // Entry point
