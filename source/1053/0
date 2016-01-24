// ---------------------------------------------------------
// Load type provider for World Bank and F# charting library
// ---------------------------------------------------------

open Samples
open Samples.Charting.DojoChart
#r "Samples.WorldBank.dll"

// ---------------------------------------------------------
// Explore university enrollment
// ---------------------------------------------------------

// Create data context connected to WorldBank
let data = WorldBank.GetDataContext()

// Compare some selected countries
let cz = data.Countries.``Czech Republic``.Indicators.``School enrollment, tertiary (% gross)``
let us = data.Countries.``United States``.Indicators.``School enrollment, tertiary (% gross)``

Chart.Combine
  [ Chart.Line(cz)
    Chart.Line(us) ]

// Compare a list of countries
let countries = 
  [ data.Countries.France
    data.Countries.Greece
    data.Countries.``Czech Republic``
    data.Countries.``United Kingdom``
    data.Countries.``European Union`` ]

Chart.Combine
  [ for country in countries ->
      let data = country.Indicators.``Central government debt, total (% of GDP)``
      Chart.Line(data, Name=country.Name) ]

// ---------------------------------------------------------
// Compare university enrolment with OECD
// ---------------------------------------------------------

// Calculate average data for all OECD members
let oecd =
  [ for c in data.Regions.``OECD members``.Countries do
      yield! c.Indicators.``School enrollment, tertiary (% gross)`` ]
  |> Seq.groupBy fst
  |> Seq.map (fun (y, v) -> y, Seq.averageBy snd v)
  |> Array.ofSeq

Chart.Combine
  [ Chart.Line(oecd)
    Chart.Line(us)
    Chart.Line(cz) ]

