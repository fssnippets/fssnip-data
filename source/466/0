open System
open System.Net
open System.Text
open System.IO
open System.Text.RegularExpressions

//just a short snippet to measure time spent in an eagerly executed function
//not gonna work with lazy function, e.g. function returning a sequence (IEnumerable)
let time jobName job = 
    let startTime = DateTime.Now;
    let returnValue = job()
    let endTime = DateTime.Now;
    printfn "%s took %d ms" jobName (int((endTime - startTime).TotalMilliseconds))
    returnValue

//goes through 2 lists in linear time and looks for equal keys
//for elements with equal keys a given function is called to produce output merged element
//NOTE:
//1. function assumes that keys in the second list are unique, otherwise results will be surprising, see (*) below
//2. function assumes both lists are ordered ascending
let rec orderedListsMerge xs ys keyExtractor merger =
    match xs, ys with
    | [],_ | _,[] -> []
    | x::xs', y::ys' ->
        let xkey = keyExtractor x
        let ykey = keyExtractor y
        if(xkey = ykey) then
            //here we move xs forward, but keep ys the same,
            //because we assume that next y will have different key while next x might still have the same key, 
            //otherwise this logic is incorrect
            (merger x y) :: orderedListsMerge xs' ys keyExtractor merger            // (*)
        elif(xkey > ykey) then
            orderedListsMerge xs ys' keyExtractor merger
        else
            orderedListsMerge xs' ys keyExtractor merger

let webRequestHtml (url : string) =
    let req = WebRequest.Create(url)
    let resp = req.GetResponse()
    let stream = resp.GetResponseStream()
    let reader = new StreamReader(stream, Encoding.GetEncoding("Windows-1251"))     //don't forget the Encoding, when you work with international documents
    let html = reader.ReadToEnd()
    resp.Close()
    html

let regexSingleLineMatch input pattern =
    Regex.Match(input, pattern, RegexOptions.Singleline).Groups.Item(1).Value

let regexMatches input pattern =
    seq { for m in Regex.Matches(input, pattern) -> m.Groups.Item(1).Value }

//only named hrefs point to poems
let extractNamedHrefs html = 
    //I tried XmlDocument here, but it doesn't work as HTML can contain some "invalid" elements like &nbsp;
    //Stand back now, I'm going to use regular expressions!
    let hrefPattern = "<a name=.* href=\"(.+?)\">.*</a>"
    regexMatches html hrefPattern


//remove all html markup from the line
let cleanupHtml text =
    let htmlTagPattern = "<.+?>"
    Regex.Replace(text, htmlTagPattern, String.Empty)

    
//remove all html markup from the line
let takeFirstLine text =
    let firstLinePattern = "(.*)"
    Regex.Match(text, firstLinePattern).Groups.Item(1).Value


type Poem(poemHref : string, title : string, lines : seq<string>) =
    let MAX_TITLE_LENGTH = 30
    member this.Href = poemHref
    member this.Title =
        let newTitle = 
            match title with
            | "* * *" -> (lines |> Seq.nth 0)
            | _ -> title
        if(newTitle.Length > MAX_TITLE_LENGTH) then
            newTitle.Substring(0, MAX_TITLE_LENGTH-3) + "..."
        else
            newTitle

    member this.Lines = 
        seq {
            for line in lines ->
                let nbspPattern = "&nbsp;"
                Regex.Replace(line, nbspPattern, "")
            }
    member this.LineTokens =
        seq { 
            for line in lines ->
                let russianWordPattern = "([а-яА-Я]+)"
                regexMatches (line.ToLower()) russianWordPattern
            }

//TODO: add more structural analysis -> handle sub-titles and personas
let producePoem poemHref poemHtml =
    //titles can be multiline, sometimes they include sub-titles
    let titlePattern = "<h1>(.+?)</h1>" 
    let linePattern = "<span class=\"line.*>(.+?)</span>"
    new Poem(
        poemHref,
        (regexSingleLineMatch poemHtml titlePattern) |> cleanupHtml |> takeFirstLine, 
        regexMatches poemHtml linePattern |> Seq.map cleanupHtml)


//check that the given link is a link to a final edition poem, not early edition to avoid duplicate texts in index
let isFinalEditionHref (href : string) =
    not (href.Contains("03edit"))

let crawlPoems =
    let domainUrl = "http://www.rvb.ru/pushkin/"
    let volumeUrlTemplate = domainUrl + "tocvol{0}.htm"
    let poemUrlTemplate = domainUrl + "{0}"

    //take only first 4 volumes -- they contain poems
    seq { for volumeNumber in 1..4 -> String.Format(volumeUrlTemplate, volumeNumber) }
        |> Seq.map webRequestHtml
        |> Seq.collect extractNamedHrefs
        |> Seq.filter isFinalEditionHref 
        |> Seq.map (fun href -> String.Format(poemUrlTemplate, href))

//        //development mode -- comment later
//        |> Seq.take 40

        //requesting individual poems
        |> Seq.map (fun href -> (producePoem href (webRequestHtml href)))
        |> Seq.cache
        

//building inversed index of tokens in poems
//so that we have a way to index (token -> poem number -> (line number,position in line))
let indexPoems (poems : seq<Poem>) = 
    poems
        |> Seq.mapi 
        (
            fun poemNumber poem -> 
                poem.LineTokens 
                    |> Seq.mapi 
                    (
                        fun lineNumber tokens ->
                            tokens
                                |> Seq.mapi 
                                (
                                    fun position token ->
                                        (token, poemNumber, lineNumber, position)
                                )
                    )
                    |> Seq.concat
        )
        |> Seq.concat

        //now we have raw list of tuples, we will turn it into ordered inversed index

        |> Seq.groupBy (fun (token, _, _, _) -> token)
        |> Seq.sortBy (fun (token, _) -> token)
        |> Seq.map 
        (
            fun (token, tuples) ->
                let poems =
                    tuples 
                        |> Seq.map ( fun (token, poemNumber, lineNumber, position) -> (poemNumber,lineNumber,position) )
                        |> Seq.groupBy (fun (poemNumber,lineNumber,position) -> poemNumber)
                        |> Seq.sortBy (fun (poemNumber, _) -> poemNumber)
                        |> Seq.map 
                        (
                            fun (poemNumber, tuples) ->
                                let linesPositions =
                                    tuples
                                        |> Seq.map (fun (poemNumber,lineNumber,position) -> (lineNumber,position))
                                        |> Seq.sortBy ( fun (lineNumber,position) -> position)
                                        |> Seq.sortBy ( fun (lineNumber,position) -> lineNumber)    //sortBy is stable according to MSDN
                                        |> Seq.toList
                                (poemNumber, linesPositions)
                        )
                        |> Seq.toList
                (token, poems)
        )
        |> Seq.toList

//token index is a subtree of full index only including poems and lines with the given token in given position
let tokenIndex fullIndex filterToken filterPosition =
    let (token, poems) = 
        fullIndex
            |> List.find (fun(token, poems) -> token=filterToken)
    poems
        |> List.map
        (
            fun (poemNumber, linesPositions) ->
                let filteredLines = linesPositions |> List.filter (fun (lineNumber, position) -> position = filterPosition)
                (poemNumber, filteredLines)
        )
        |> List.filter (fun (poemNumber, linesPositions) -> not (Seq.isEmpty linesPositions))


//intersect current index with token index
//we want to only keep tokens and poems which are present in the token index (which is a subtree of full index, see above)
let intersectIndex currentIndex tokenIndex =
    currentIndex
        |> List.map
        (
            fun (token, poems) ->
                let mergePoems currentPoems tokenPoems =
                    let mergeLinesPositions currentLinesPositions tokenLinesPositions =
                        let keyExtractor = (fun (lineNumber, _) -> lineNumber)
                        let merger = (fun (currentLineNumber, currentPosition) (_,_) -> (currentLineNumber, currentPosition))
                        orderedListsMerge currentLinesPositions tokenLinesPositions keyExtractor merger

                    let keyExtractor = (fun (poemNumber, _) -> poemNumber)
                    let merger = (fun (currentPoemNumber, currentLinesPositions) (_, tokenLinesPositions) -> (currentPoemNumber, mergeLinesPositions currentLinesPositions tokenLinesPositions))
                    orderedListsMerge currentPoems tokenPoems keyExtractor merger
                        |> List.filter (fun (poemNumber, linesPositions) -> not (List.isEmpty linesPositions))

                (token, mergePoems poems tokenIndex)
        )
        |> List.filter (fun (token, poems) -> not (List.isEmpty poems))


// The main function to query reverse index
// index -- index per se, we assume that the index is already filtered by caller using intersect\tokenFilter
// findPosition -- number of position to search tokens for
//                 the query function will return a list of terms that can be in this position
// count -- number of most frequent terms to return
let queryIndex index findPosition count =
    index 
        |> List.map
        (
            fun (token, poems) ->
                let tokenFreq = 
                    poems
                        |> List.sumBy
                        (
                            fun (_, linesPositions) ->
                                linesPositions
                                    |> List.sumBy
                                    (
                                        fun (lineNumber, position) ->
                                            if (position = findPosition) then 1 else 0
                                    )
                        )
                (token, tokenFreq)
        )
        |> Seq.filter (fun (token, tokenFreq) -> tokenFreq > 0)
        |> Seq.sortBy (fun (token, tokenFreq) -> -tokenFreq)
        |> Seq.zip [1..count] // Seq.take fails if there is less than "count" elements
        |> Seq.map (fun (index, element) -> element)
        |> Seq.toList

//acquire first poem for given token and position
//used to resolve single query result token into poems
let getPoemResult index findToken findPosition =
    let (token, poems) = 
        index
            |> List.find (fun (token, poems) -> token = findToken)
    
    poems
        |> List.collect
        (
            fun (poemNumber, linesPositions) ->
                linesPositions
                    |> List.filter (fun (lineNumber, position) -> position = findPosition)
                    |> List.map (fun (lineNumber, position) -> (poemNumber, lineNumber))
        )
        |> Seq.nth 0


type QueryResult =
    //token + count
    | LineVariant       of string*int
    //poemNumber, lineNumber
    | SinglePoem    of int*int

//TODO: identical strings currently will not be resolved to their poems

//this is a wrapper around query index that will perform the same action,
//but the result will be translated and wrapped into QueryResult type
//single result tokens will be returned as (poemNumber, lineNumber) tuple
let wrappedQueryIndex filteredIndex searchPosition count =
    queryIndex filteredIndex searchPosition count
        |> List.map 
        (
            fun (token, count) ->
                match count with
                | 1 -> SinglePoem(getPoemResult filteredIndex token searchPosition)
                | _ -> LineVariant(token,count)
        )


type PrettyResult =
    | PrettyLineVariant       of string*int
    | PrettySinglePoem        of string*string*int*string

let prettifyQueryResults queryResults poems =
    queryResults
        |> List.map
        (
            fun result ->
                match result with
                | SinglePoem (poemNumber, lineNumber) -> 
                    let (poem : Poem) = 
                        poems
                            |> Seq.nth poemNumber
                    let line =
                        poem.Lines
                            |> Seq.nth lineNumber
                    PrettySinglePoem(poem.Title, poem.Href, lineNumber, line)
                | LineVariant (token, count) -> PrettyLineVariant(token,count)
        )

type PushkinTreeNode =
    | VariantNode     of string*int*seq<PushkinTreeNode>
    | PoemNode        of string*string*int*string

let createPushkinTree pushkinPoems poemsIndex count =
    let rec createTreeLevel count currentQuery currentIndex =
        let searchPosition = List.length currentQuery
        let queryResult = wrappedQueryIndex currentIndex searchPosition count
        let prettyResult = prettifyQueryResults queryResult pushkinPoems
        prettyResult
            |> List.map
            (
                fun result ->
                    match result with
                    | PrettySinglePoem (title, href, lineNumber, line) -> PoemNode(title, href, lineNumber, line)
                    | PrettyLineVariant (token, freq) -> VariantNode(token, freq, createTreeLevel count (currentQuery @ [token]) (intersectIndex currentIndex (tokenIndex currentIndex token searchPosition)))
            )
    createTreeLevel count [] poemsIndex

let resultsToHtml pushkinTree =
    let rec treeToHtml tree currentPath (currentNumber:int) startingNumber =
        let zippedTreeLevel =
            tree
                |> Seq.zip (Seq.initInfinite (fun i -> startingNumber+i))

        let pathToString path =
            let parts = 
                path
                    |> Seq.map (fun x -> "'"+x+"'")
            String.Join(",", parts)

        let thisLevelStart = String.Format("<div class=\"x\"><table id=\"{0}\" class=\"p\">", currentNumber) + Environment.NewLine
        let thisLevelTable = 
            zippedTreeLevel
                |> Seq.fold
                (
                    fun acc treeNode ->
                        match treeNode with
                        | (number, PoemNode (title, href, lineNumber, line)) ->
                            acc + String.Format("<tr><td><span class=line>{0}</span> &#8658; <a target=blank class=fromlink href=\"{1}\">{2}</a>, строка {3}</tr>",line, href, title, lineNumber+1) + Environment.NewLine
                        | (number, VariantNode (token, freq, subtree)) ->
                            acc + String.Format("<tr id=\"r{0}\"><td>{1} &#8658; <span class=\"lv\" onClick=\"x([{2}])\">{3}</span></td></tr>", number, token, (pathToString (currentPath@[string(number)])), freq) + Environment.NewLine
                ) ""
        let thisLevelEnd = @"</table></div>" + Environment.NewLine + Environment.NewLine

        let thisLevelOutput = (thisLevelStart+thisLevelTable+thisLevelEnd)

        let levelLength = 
            tree
                |> Seq.length

        let (subTreeCount, subTreeOutput) =
            zippedTreeLevel 
                |> Seq.fold
                (
                    fun (acc, result) treeNode ->
                        match treeNode with
                        | (number, PoemNode (title, href, lineNumber, line)) ->
                            (acc, result)
                        | (number, VariantNode (token, freq, subtree)) ->
                            let (subTreeCount, subTreeOutput) = treeToHtml subtree (currentPath@[string(number)]) number (startingNumber+acc+levelLength)
                            (acc + subTreeCount, result + subTreeOutput)
                ) (0, "")

        (levelLength + subTreeCount, thisLevelOutput + subTreeOutput)
       
    let (_, content) = treeToHtml pushkinTree ["0"] 0 1
    content

let outputResultsToFile (content:string) =
    let templateFile = "template.htm"
    let outputFile = "output.htm"
    let templateReplacePattern = "#HERE_GOES_CONTENT#"
    let templateHtml = File.ReadAllText(templateFile)
    let resultHtml = Regex.Replace(templateHtml, templateReplacePattern, content)
    File.WriteAllText(outputFile, resultHtml)


let poems = crawlPoems //lazy operation, so we can't time it here, we do it the in next line
printfn "Crawled %d poems" (time "Crawling poems" (fun() -> poems |> Seq.length))
printfn "Crawled %d lines" (poems |> Seq.sumBy ( fun poem -> poem.Lines |> Seq.length))
let poemIndex = time "Indexing poems" (fun () -> poems |> indexPoems)
printfn "Index contains %d terms" poemIndex.Length
let tree = time "Generating result tree" (fun () -> createPushkinTree poems poemIndex 20) 
let htmlContent = time "Generating html content" (fun () -> resultsToHtml tree)
time "Output content" (fun() -> outputResultsToFile htmlContent)

//let queryResult = queryIndex poemIndex ["но"] 10
//let prettyResult = prettifyQueryResults queryResult poems

