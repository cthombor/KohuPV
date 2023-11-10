#

# https://cran.r-project.org/web/packages/ggiraphExtra/vignettes/ggPredict.html
# devtools::install_github("cardiomoon/ggiraphExtra")

if (!("tidyverse" %in% (.packages()))) library(tidyverse)

require(ggiraph) # interactive graphs
require(ggiraphExtra) # visualise predictions from multiple regression models

load("data/gw.rda")
load("data/sc.rda")
load("data/sc.all.rda")
load("data/recent0.daily.rda")
load("data/trina270.daily.rda")

gw_daily <- gw |>
  mutate(PeriodEnd.NZ =
           force_tz(
             date(dateTime) + hour(dateTime) * dhours(1) +
               trunc(minute(dateTime) / 30) * 30 * dminutes(1)
           ),
         tz = "NZ") |>
  group_by(date(dateTime)) |>
  mutate(
    V.MPPT.sd = sd(V.MPPT.1.V.),
    I.MPPT.sd = sd(I.MPPT.1.A.),
    Power.W.sd = sd(Power.W.),
    Inverter.Temperature.max = max(Temperature.)
  ) |>
  distinct(genDay1, pick(V.MPPT.sd:Inverter.Temperature.max)) |>
  ungroup() |>
  rename(Date = 'date(dateTime)')

#Peimar 300W
gw_r0_daily <- inner_join(gw_daily, recent0.daily, by=c("Date")) |>
  rename(Actual.kWh = genDay1) |>
  rename(Predicted.kWh = kWh) |>
  mutate(diff.kWh = Predicted.kWh - Actual.kWh)

summary(gw_r0_daily$Predicted.kWh)
summary(gw_r0_daily$Actual.kWh)
summary(gw_r0_daily$diff.kWh)
cor(gw_r0_daily$Predicted.kWh,gw_r0_daily$Actual.kWh)

gw_r0_daily |>
  select(c("Date","Predicted.kWh","Actual.kWh")) |>
  pivot_longer(cols=2:3) |>
  ggplot(aes(x=Date, y=value, colour=name)) +
  geom_line()

gw_model <- lm(Actual.kWh ~ Predicted.kWh, gw_r0_daily)
summary(gw_model)

#Trina 270W, Goodwe 2000-NS
gw_r0_daily <- inner_join(gw_r0_daily, trina270.daily, by=c("Date")) |>
  rename(Predicted.Trina270.kWh = kWh) |>
  mutate(diff.Trina270.kWh = Predicted.Trina270.kWh - Actual.kWh)

summary(gw_r0_daily$Predicted.Trina270.kWh)
summary(gw_r0_daily$Actual.kWh)
summary(gw_r0_daily$diff.kWh)
cor(gw_r0_daily$Predicted.Trina270.kWh,gw_r0_daily$Actual.kWh)

gw_r0_daily |>
  select(c("Date","Predicted.Trina270.kWh","Actual.kWh")) |>
  pivot_longer(cols=2:3) |>
  ggplot(aes(x=Date, y=value, colour=name)) +
  geom_line()

gw_model <- lm(Actual.kWh ~ Predicted.Trina270.kWh, gw_r0_daily)
summary(gw_model)



gw_HH <- gw |>
  mutate(PeriodEnd.NZ = force_tz(
    date(dateTime) + hour(dateTime) * dhours(1) +
      trunc(minute(dateTime) / 30) * 30 * dminutes(1)
  ),
  tz = "NZ") |>
  group_by(PeriodEnd.NZ) |>
  mutate(
    V.MPPT.sd = sd(V.MPPT.1.V.),
    I.MPPT.sd = sd(I.MPPT.1.A.),
    Power.W.sd = sd(Power.W.),
    Temperature.max = max(Temperature.)
  ) |>
  distinct(genHalfHour1, pick(V.MPPT.sd:Temperature.max)) |>
  ungroup()

pv_data <- inner_join(gw_HH, sc, by = join_by(PeriodEnd.NZ))

pv_model <- lm(genHalfHour1 ~ 0 + PvEstimate, pv_data)
summary(pv_model)

ggplot(pv_data, aes(genHalfHour1, PvEstimate)) +
  geom_boxplot(aes(group = cut_interval(genHalfHour1, n = 10)))

pv_model <- lm(genHalfHour1 ~ 0 + PvEstimate + Power.W.sd, pv_data)
summary(pv_model)

pv_model <- lm(
  genHalfHour1 ~ 0 + PvEstimate + V.MPPT.sd + Power.W.sd + Temperature.max,
  pv_data)
summary(pv_model)

pv_model <- lm(
  genHalfHour1 ~ 0 + PvEstimate + V.MPPT.sd,
  pv_data)
summary(pv_model)
ggPredict(pv_model,se=TRUE,interactive=TRUE)
ggPredict(pv_model,se=TRUE,interactive=TRUE,colorn=8)



