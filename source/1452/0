[<Measure>] type eur;       //defino la dimension dinero
[<Measure>] type month;     //defino el tiempo en meses
[<Measure>] type day;       //defino tiempo en dias
[<Measure>]
type hour = 
  static member workDay = 8.0<hour/day>;    //defino el tiempo en horas y la conversion por dia laborable
[<Measure>] 
type min = 
  static member perHour = 60.0<min hour^-1>;    //defino el tiempo en minutos y la conversion a horas

let hourRate = 60.0<eur/hour>;      //defino la tarifa horaria

let avgMonthlyEffort = 20.0<day/month> * hour.workDay //cuantas horas de esfuerzo tiene un mes con 20 dias laborables?
let avgMonthlyCost = avgMonthlyEffort * hourRate //cuanto cuesta segun la tarifa?

let monthCost = 8000.0<eur/month> //defino que un mes me ha costado 8000 euros
let hourCost = monthCost / 20.0<day/month> / hour.workDay //¿que tarifa me han aplicado?