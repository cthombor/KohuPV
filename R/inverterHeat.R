if (!("tidyverse" %in% (.packages()))) library(tidyverse)
require(ggiraph) # interactive graphs
require(ggiraphExtra) # visualise predictions from multiple regression models

load("data/gw.rda")
load("data/recent.peimarsim.hourly.horizon.rda")
load("data/recent.trinasim.hourly.rda")

gw.heat <- gw |>
  mutate(
    delta.T = (Temperature. - lag(Temperature.)),
    delta.t = timeStepLag / dhours(1),
    # inverter temperature rise, in degrees/hour
    # we don't attempt to model overnight temperature drops
    dT.dt = if_else(delta.t==0 | delta.t > dhours(2), NA, delta.T / delta.t),

        # inverter inefficiency is not self-reported, making me wonder whether
    # it's the output power or the input power that's inaccurate
    # invLoss.W = (V.MPPT.1.V. * I.MPPT.1.A.) - (Ua.V. * I.AC.1.A.),
    # summary(gw.heat$invLoss.W)
    # Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
    # -301.140  -18.900    0.000   -6.086   15.880  628.600
    # invLoss.kWh = invLoss.W *
    #  dt / time_length(dhours(1), unit = "seconds") / 1000,

    # my rough-fit of the Davila-Gomez model of inverter inefficiency
    # (http://dx.doi.org/10.1016/j.simpat.2014.08.001)
    # to a grid-tied GW3000-NS
    RS.0 = 0.5,
    RP.0 = 1600,
    loading = 1,
    term1.0 = 2 * RS.0 * loading * Power.W. / (V.MPPT.1.V. * V.MPPT.1.V.),
    term2.0 = 1 - sqrt(1 - 4 *
                       (RS.0 / (
                         V.MPPT.1.V. * V.MPPT.1.V.
                       )) *
                       (loading * Power.W. + (Ua.V. * Ua.V. / RP.0))),
    DGEff.0 = ifelse(Working.Mode == "Fault", 0, term1.0 / term2.0),
    DGPower.0 = ifelse(Working.Mode == "Fault", 0, Power.W. * (1 - DGEff.0)),
    DGEnergy.0.kWh =
      DGPower.0 * dt / time_length(dhours(1), unit = "seconds") / 1000,

    # perturb & observe -- is this a better fit?
#    RS = 0.98,
#    RP = 600,
    RS = 0.4,
    RP = 2000,
    term1 = 2 * RS * loading * Power.W. / (V.MPPT.1.V. * V.MPPT.1.V.),
    term2 = 1 - sqrt(1 - 4 *
                       (RS / (
                         V.MPPT.1.V. * V.MPPT.1.V.
                       )) *
                       (loading * Power.W. + (Ua.V. * Ua.V. / RP))),
    DGEff = ifelse(Working.Mode == "Fault", 0, term1 / term2),
    DGPower = ifelse(Working.Mode == "Fault", 0, Power.W. * (1 - DGEff)),
    DGEnergy.kWh =
      DGPower * dt / time_length(dhours(1), unit = "seconds") / 1000
  ) |>
  select(!(term1:term2) & !(term1.0:term2.0))

gw.heat <- gw.heat |>
  group_by(Date) |>
  mutate(DGEnergy.Day.kWh = sum(DGEnergy.kWh)) |>
  mutate(
    netGenDay = genDay - DGEnergy.Day.kWh,
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
    cumDGEnergyDay =
      cumsum(ifelse(is.na(DGEnergy.kWh),
                    0,
                    DGEnergy.kWh)) + DGEnergy.kWh * 0
  )

gw.heat <- gw.heat |>
  group_by(Date, Hour) |>
  mutate(
    DGEnergy.0.Hour.kWh = sum(DGEnergy.0.kWh, na.rm = TRUE),
    DGEnergy.Hour.kWh = sum(DGEnergy.kWh, na.rm = TRUE)) |>
  mutate(
    mean.DG.Power.0.Hour = DGEnergy.0.Hour.kWh / sum(delta.t) * 1000,
    mean.DG.Power.Hour = DGEnergy.Hour.kWh / sum(delta.t) * 1000,
    netGenHour = genHour - DGEnergy.Hour.kWh
  ) |>
  group_by(Date, timeOfDay.qhour) |>
  mutate(
    DGEnergy.QHour.0.kWh = sum(DGEnergy.0.kWh, na.rm=TRUE),
    DGEnergy.QHour.kWh = sum(DGEnergy.kWh, na.rm=TRUE)) |>
  mutate(
    mean.DG.Power.0.QHour = DGEnergy.QHour.0.kWh / sum(delta.t) * 1000,
    mean.DG.Power.QHour = DGEnergy.QHour.kWh / sum(delta.t) * 1000,
    netGenQHour = genQuarterHour - DGEnergy.QHour.kWh
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
    max.V.MPPT = max(V.MPPT.1.V., na.rm=TRUE),
    mean.I.MPPT = mean(I.MPPT.1.A., na.rm=TRUE),
    mean.Ua.MPPT = mean(Ua.V., na.rm=TRUE),
    mean.I.AC = mean(I.AC.1.A., na.rm=TRUE),
    mean.F.AC = mean(F.AC.1.Hz., na.rm=TRUE),
    mean.Power.W = mean(Power.W., na.rm=TRUE),
    mean.H.Total = mean(H.Total.h., na.rm=TRUE),
    mean.DGPower = first(mean.DG.Power.Hour),
    DGEnergy = first(DGEnergy.Hour.kWh),
    genHour = first(genHour),
    cumGenDay = last(cumGenDay),
    netGenHour = first(netGenHour),
    DGEff = netGenHour / genHour,
    cumDGEnergyDay = last(cumDGEnergyDay),
    Source = first(Source),
    shadowScan = first(shadowScan),
    Year = first(Year),
    Month = first(Month),
    Day = first(Day),
    Hour = first(Hour)
  ) |>
  mutate(
    sd.DGEff = sd(DGEff, na.rm=TRUE),
    lag1H.DG.Power =
      if_else(lag(Hour) == Hour - 1,
              lag(mean.DGPower),
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
    first.T.day = first(first.T.day),
    first.qhour.day = first(first.qhour.day),
    mean.V.MPPT = mean(V.MPPT.1.V., na.rm=TRUE),
    sd.V.MPPT = sd(V.MPPT.1.V., na.rm=TRUE),
    max.V.MPPT = max(V.MPPT.1.V., na.rm=TRUE),
    mean.I.MPPT = mean(I.MPPT.1.A., na.rm=TRUE),
    mean.Ua.MPPT = mean(Ua.V., na.rm=TRUE),
    mean.I.AC = mean(I.AC.1.A., na.rm=TRUE),
    mean.F.AC = mean(F.AC.1.Hz., na.rm=TRUE),
    mean.Power.W = mean(Power.W., na.rm=TRUE),
    mean.H.Total = mean(H.Total.h., na.rm=TRUE),
    mean.DGPower = first(mean.DG.Power.QHour),
    DGEnergy = first(DGEnergy.QHour.kWh),
    genQHour = first(genQuarterHour),
    cumGenDay = last(cumGenDay),
    netGenQHour = first(netGenQHour),
    DGEff = netGenQHour / genQuarterHour,
    cumDGEnergyDay = last(cumDGEnergyDay),
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
    sd.DGEff = sd(DGEff, na.rm=TRUE),
    lagQH.DG.Power =
      if_else(lag(timeOfDay.qhour) == timeOfDay.qhour - 0.25,
              lag(mean.DGPower),
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
    max.V.MPPT = max(V.MPPT.1.V., na.rm=TRUE),
    mean.I.MPPT = mean(I.MPPT.1.A., na.rm=TRUE),
    mean.Ua.MPPT = mean(Ua.V., na.rm=TRUE),
    mean.I.AC = mean(I.AC.1.A., na.rm=TRUE),
    mean.F.AC = mean(F.AC.1.Hz., na.rm=TRUE),
    mean.Power.W = mean(Power.W., na.rm=TRUE),
    mean.H.Total = mean(H.Total.h., na.rm=TRUE),
    mean.DGPower = mean(DGPower),
    genDay = first(genDay),
    DGEnergy = first(DGEnergy.Day.kWh),
    netGen = first(netGenDay),
    DGEff = netGen / genDay,
    sd.DGEff = sd(DGEff, na.rm=TRUE),
    Source = first(Source),
    shadowScan = first(shadowScan),
    Year = first(Year,),
    Month = first(Month),
    Day = first(Day),
  ) |>
  group_by()

#simulated Peimar 300W with geo horizon (vcc), recent 12-month period
gw.heat.hour.recent <- inner_join(gw.heat.hour,
                                  recent.peimarsim.hourly.horizon,
                                  by=c("Year", "Month", "Day", "Hour")) |>
  rename(Actual.hour.kWh = genHour) |>
  rename(PredictedPeimar.hour.kWh = E_Avail) |>
  mutate(errPred = PredictedPeimar.hour.kWh - Actual.hour.kWh) |>
  relocate(PredictedPeimar.hour.kWh : errPred, .after = Actual.hour.kWh)

#simulated Trina 270W
gw.heat.hour.recent <- inner_join(gw.heat.hour.recent,
                                  recent.trinasim.hourly,
                                  by=c("Year", "Month", "Day", "Hour")) |>
  rename(PredictedTrina.hour.kWh = E_Avail) |>
  mutate(errTrinaPred = PredictedTrina.hour.kWh - Actual.hour.kWh,
         QYear = factor(quarter(ymd(Date), type = "year.quarter"))
  ) |>
  relocate(PredictedTrina.hour.kWh : errTrinaPred, .after = Actual.hour.kWh)

print(paste0("Actual output is ",
             round(sum(gw.heat.hour.recent$Actual.hour.kWh)
                   / sum(gw.heat.hour.recent$PredictedPeimar.hour.kWh),3)*100,
             "% of predicted output with 300W Peimar panels,"))
print(paste0("and ",
             round(sum(gw.heat.hour.recent$Actual.hour.kWh)
                   / sum(gw.heat.hour.recent$PredictedTrina.hour.kWh),3)*100,
             "% of predicted output with 270W Trina panels and Goodwe 2000"))
rm(gwx,gwy)

usethis::use_data(gw.heat, overwrite=TRUE)
usethis::use_data(gw.heat.day, overwrite=TRUE)
usethis::use_data(gw.heat.hour, overwrite=TRUE)
usethis::use_data(gw.heat.qhour, overwrite=TRUE)
usethis::use_data(gw.heat.hour.recent, overwrite=TRUE)


