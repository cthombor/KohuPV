if (!("tidyverse" %in% (.packages()))) library(tidyverse)
require(ggiraph) # interactive graphs
require(ggiraphExtra) # visualise predictions from multiple regression models

load("data/gw.rda")

gw.heat <- gw |>
  mutate(
    delta.T = (Temperature. - lag(Temperature.)),
    delta.t = timeStepLag / dhours(1),
    # inverter temperature rise, in degrees/hour
    # we don't attempt to model overnight temperature drops
    dT.dt = if_else(delta.t==0 | delta.t > dhours(2), NA, delta.T / delta.t),
    # my rough-fit of Chevelet's model to a grid-tied GW3000-NS
    RS = 0.5,
    RP = 1600,
    loading = 0.95,
    term1 = 2 * RS * loading * Power.W. / (V.MPPT.1.V. * V.MPPT.1.V.),
    term2 = 1 - sqrt(1 - 4 *
                       (RS / (
                         V.MPPT.1.V. * V.MPPT.1.V.
                       )) *
                       (loading * Power.W. + (Ua.V. * Ua.V. / RP))),
    CheveletEff = ifelse(Working.Mode == "Fault", 0, term1 / term2),
    CheveletPower = ifelse(Working.Mode == "Fault",
                           0, Power.W. * (1 - CheveletEff)),
#    dt = if_else(timeStepLag > dseconds(61),
#                61, ## the usual timestep
#                timeStepLag / dseconds(1)),
    CheveletEnergy.kWh =
      CheveletPower * dt / time_length(dhours(1), unit = "seconds") / 1000
  ) |>
  select(!(term1:term2))

gw.heat <- gw.heat |>
  group_by(Date) |>
  mutate(CheveletEnergy.Day.kWh = sum(CheveletEnergy.kWh)) |>
  mutate(
    netGenDay = genDay - CheveletEnergy.Day.kWh,
    avgEfficiency.Day = netGenDay / genDay,
    timeOfDay.qhour = trunc(4.0 * as.double(
      dateTime - force_tz(
        as.Date(Date, tz = "NZ")
      ))) / 4.0,
    first.T.day = first(Temperature.))

gw.heat <- gw.heat |>
  mutate(
    first.qhour.day = first(timeOfDay.qhour),
    # https://stackoverflow.com/questions/25576358/calculate-cumsum-while-ignoring-na-values
    cumGenDay =
      cumsum(ifelse(is.na(Power.W. * dt),
                    0,
                    (Power.W. * dt / 3600) / 1000)) + (Power.W. * dt) * 0,
    cumChevEnergyDay =
      cumsum(ifelse(is.na(CheveletEnergy.kWh),
                    0,
                    CheveletEnergy.kWh)) + CheveletEnergy.kWh * 0
  )

gw.heat <- gw.heat |>
  group_by(Date, Hour) |>
  mutate(
    CheveletEnergy.Hour.kWh = sum(CheveletEnergy.kWh, na.rm = TRUE)) |>
  mutate(
    mean.Chevelet.Power.Hour = CheveletEnergy.Hour.kWh / sum(delta.t) * 1000,
    netGenHour = genHour - CheveletEnergy.Hour.kWh
  ) |>
  group_by(Date, timeOfDay.qhour) |>
  mutate(
    CheveletEnergy.QHour.kWh = sum(CheveletEnergy.kWh, na.rm=TRUE)) |>
  mutate(
    mean.Chevelet.Power.QHour = CheveletEnergy.QHour.kWh / sum(delta.t) * 1000,
    netGenQHour = genQuarterHour - CheveletEnergy.QHour.kWh
    ) |>
  ungroup()

# inspect the wonky timesequence
gwy <- filter(gw.heat, Date=="2018-05-24")

gw.heat.hour <- gw.heat |>
  group_by(Date, Hour) |>
  summarise(
    delta.t = sum(delta.t, na.rm=TRUE),
    delta.T = sum(delta.T, na.rm=TRUE),
    max.T = max(Temperature., na.rm=TRUE),
    mean.T = mean(Temperature., na.rm=TRUE),
    min.T = min(Temperature., na.rm=TRUE),
    first.T.day = first(first.T.day),
    first.qhour.day = first(first.qhour.day),
    mean.V.MPPT = mean(V.MPPT.1.V., na.rm=TRUE),
    sd.V.MPPT = sd(V.MPPT.1.V., na.rm=TRUE),
    mean.I.MPPT = mean(I.MPPT.1.A., na.rm=TRUE),
    mean.Ua.MPPT = mean(Ua.V., na.rm=TRUE),
    mean.I.AC = mean(I.AC.1.A., na.rm=TRUE),
    mean.F.AC = mean(F.AC.1.Hz., na.rm=TRUE),
    mean.Power.W = mean(Power.W., na.rm=TRUE),
    mean.H.Total = mean(H.Total.h., na.rm=TRUE),
    mean.CheveletPower = first(mean.Chevelet.Power.Hour),
    CheveletEnergy = first(CheveletEnergy.Hour.kWh),
    genHour = first(genHour),
    cumGenDay = last(cumGenDay),
    netGenHour = first(netGenHour),
    CheveletEff = netGenHour / genHour,
    cumChevEnergyDay = last(cumChevEnergyDay),
    Source = first(Source),
    shadowScan = first(shadowScan),
    Year = first(Year),
    Month = first(Month),
    Day = first(Day),
    Hour = first(Hour)
  ) |>
  mutate(
    sd.ChevEff = sd(CheveletEff, na.rm=TRUE),
    lag1H.Chev.Power =
      if_else(lag(Hour) == Hour - 1,
              lag(mean.CheveletPower),
              NA),
    dT.dt = if_else(delta.t == 0, NA, delta.T / delta.t),
    dateTime = ymd(Date, tz = "NZ") + dhours(Hour)
  ) |>
  group_by()

# inspect a one-day timesequence
gwx <- filter(gw.heat.hour, Date=="2018-05-24")

gw.heat.qhour <- gw.heat |>
  group_by(Date, timeOfDay.qhour) |>
  summarise(
    delta.t = sum(delta.t, na.rm=TRUE),
    delta.T = sum(delta.T, na.rm=TRUE),
    max.T = max(Temperature., na.rm=TRUE),
    mean.T = mean(Temperature., na.rm=TRUE),
    min.T = min(Temperature., na.rm=TRUE),
    mean.V.MPPT = mean(V.MPPT.1.V., na.rm=TRUE),
    sd.V.MPPT = sd(V.MPPT.1.V., na.rm=TRUE),
    mean.I.MPPT = mean(I.MPPT.1.A., na.rm=TRUE),
    mean.Ua.MPPT = mean(Ua.V., na.rm=TRUE),
    mean.I.AC = mean(I.AC.1.A., na.rm=TRUE),
    mean.F.AC = mean(F.AC.1.Hz., na.rm=TRUE),
    mean.Power.W = mean(Power.W., na.rm=TRUE),
    mean.H.Total = mean(H.Total.h., na.rm=TRUE),
    mean.CheveletPower = first(mean.Chevelet.Power.QHour),
    genQHour = first(genQuarterHour),
    CheveletEnergy = first(CheveletEnergy.QHour.kWh),
    netGenQHour = first(netGenQHour),
    CheveletEff = netGenQHour / genQHour,
    cumGenDay = last(cumGenDay),
    cumChevEnergyDay = last(cumChevEnergyDay),
    Source = first(Source),
    shadowScan = first(shadowScan),
    Year = first(Year),
    Month = first(Month),
    Day = first(Day),
    Hour = first(Hour),
    first.T.day = first(first.T.day),
    first.qhour.day = first(first.qhour.day)
  ) |>
  mutate(
    sd.ChevEff = sd(CheveletEff, na.rm=TRUE),
    lagQH.Chev.Power =
      if_else(lag(timeOfDay.qhour) == timeOfDay.qhour - 0.25,
              lag(mean.CheveletPower),
              NA),

    dT.dt = if_else(delta.t == 0, NA, delta.T / delta.t),
    dateTime = ymd(Date, tz = "NZ") + dhours(timeOfDay.qhour)
  ) |>
  group_by()
gwy <- filter(gw.heat.qhour, Date=="2018-05-24")

gw.heat.day <- gw.heat |>
  group_by(Date) |>
  summarise(
    max.T = max(Temperature., na.rm=TRUE),
    mean.T = mean(Temperature., na.rm=TRUE),
    min.T = min(Temperature., na.rm=TRUE),
    mean.V.MPPT = mean(V.MPPT.1.V., na.rm=TRUE),
    sd.V.MPPT = sd(V.MPPT.1.V., na.rm=TRUE),
    mean.I.MPPT = mean(I.MPPT.1.A., na.rm=TRUE),
    mean.Ua.MPPT = mean(Ua.V., na.rm=TRUE),
    mean.I.AC = mean(I.AC.1.A., na.rm=TRUE),
    mean.F.AC = mean(F.AC.1.Hz., na.rm=TRUE),
    mean.Power.W = mean(Power.W., na.rm=TRUE),
    mean.H.Total = mean(H.Total.h., na.rm=TRUE),
    mean.CheveletPower = mean(CheveletPower, na.rm=TRUE),
    genDay = first(genDay),
    CheveletEnergy = first(CheveletEnergy.Day.kWh),
    netGen = first(netGenDay),
    CheveletEff = netGen / genDay,
    sd.ChevEff = sd(CheveletEff, na.rm=TRUE),
    Source = first(Source),
    shadowScan = first(shadowScan),
    Year = first(Year,),
    Month = first(Month),
    Day = first(Day),
   ) |>
  group_by()

rm(gwx,gwy)

usethis::use_data(gw.heat, overwrite=TRUE)
usethis::use_data(gw.heat.day, overwrite=TRUE)
usethis::use_data(gw.heat.hour, overwrite=TRUE)
usethis::use_data(gw.heat.qhour, overwrite=TRUE)


