if (!("tidyverse" %in% (.packages()))) library(tidyverse)
require(ggiraph) # interactive graphs
require(ggiraphExtra) # visualise predictions from multiple regression models

load("data/gw.rda")
load("data/sc.rda")
load("data/sc.all.rda")
load("data/sc.hist.rda")
load("data/recent0.daily.rda")
load("data/trina270.daily.rda")
load("data/gw.heat.hour.recent")
load("data/firstyear.peimarsim.hourly.horizon.rda")
load("data/augsep23.peimarsim.hourly.horizon.rda")
load("data/recent.trinasim.hourly.rda")
load("data/gw.heat.rda")
load("data/gw.heat.day.rda")
load("data/gw.heat.hour.rda")
load("data/gw.heat.qhour.rda")

make_modelp0 <- function(data){
  lm(Actual.hour.kWh ~ PredictedPeimar.hour.kWh + 0, data)
}

make_modelp <- function(data){
  lm(Actual.hour.kWh ~ PredictedPeimar.hour.kWh, data)
}


gw.pivot <- gw.heat.hour.recent |> # prep for stacked barplot
  pivot_longer(
    cols = c("errPred","Actual.hour.kWh"),
    names_to = "measure",
    values_to = "kWh"
  ) |>
  mutate(measure = forcats::fct_rev(measure))
gw.pivot |>
  ggplot(aes(fill = measure, x = Hour, y = kWh)) +
  stat_summary(fun.data = "mean_cl_boot",
               geom = "bar",
               position = "stack") +
  facet_wrap(gw.pivot$Month)

gw.heat.hour.recent |>
  ggplot(aes(Actual.hour.kWh, errPred)) +
  geom_boxplot(aes(group=cut_number(Actual.hour.kWh, 6)))

gw.heat.hour.recent |>
  filter(Actual.hour.kWh < 0.2) |>
  ggplot(aes(PredictedPeimar.hour.kWh, errPred)) +
  geom_boxplot(aes(group=cut_number(PredictedPeimar.hour.kWh, 4)))

gw.heat.hour.recent |>
  ggplot(aes(Hour, errPred)) +
  geom_boxplot(aes(group=cut_width(Hour,1)))

#simulated Trina 270W

gw.heat.hour.recent |>
  ggplot(aes(Hour, errTrinaPred)) +
  geom_boxplot(aes(group=cut_width(Hour,1)))

gw.heat.hour.recent |>
  ggplot(aes(Hour, Actual.hour.kWh/PredictedTrina.hour.kWh)) +
  geom_boxplot(aes(group=cut_width(Hour,1)))

gw.heat.hour.recent |>
#  filter(Hour > 8 & Hour < 19) |>
  mutate(Hour = as.factor(Hour)) |>
  ggplot(aes(Hour, PredictedTrina.hour.kWh - Actual.hour.kWh)) +
  geom_boxplot()

gw.heat.hour.recent |>
#  filter(Hour > 9 & Hour < 18) |>
  mutate(Hour = as.factor(Hour)) |>
  ggplot(aes(mean.I.MPPT, errTrinaPred)) +
  geom_boxplot(aes(group=cut_width(mean.I.MPPT,1)))

gw.heat.hour.recent |>
  #  filter(Hour > 9 & Hour < 18) |>
  mutate(Hour = as.factor(Hour)) |>
  ggplot(aes(max.V.MPPT, errTrinaPred)) +
  geom_boxplot(aes(group=cut_number(max.V.MPPT,5)))

gw.heat.hour.recent |>
  filter(Hour == 13) |>
  ggplot(aes(mean.I.MPPT, PredictedTrina.hour.kWh - Actual.hour.kWh)) +
  geom_boxplot(aes(group=cut_width(mean.I.MPPT,1))) +
  labs(title="errTrina by I.MPPT, at Hour == 13")

gw.heat.hour.recent |>
  filter(Hour == 13) |>
  ggplot(aes(max.V.MPPT, errTrinaPred)) +
  geom_boxplot(aes(group=cut_number(max.V.MPPT,5))) +
  labs(title="errTrina by max.V.MPPT, at Hour == 13")

gw.heat.hour.recent |>
  filter(Hour == 17) |>
  ggplot(aes(max.V.MPPT, errTrinaPred)) +
  geom_boxplot(aes(group=cut_number(max.V.MPPT,5)))+
  labs(title="errTrina by max.V.MPPT, at Hour == 17")

gw.heat.hour.recent |>
  filter(Hour == 10) |>
  ggplot(aes(max.V.MPPT, errTrinaPred)) +
  geom_boxplot(aes(group=cut_number(max.V.MPPT,5)))+
  labs(title="errTrina by max.V.MPPT, at Hour == 10")

gw.heat.hour.recent |>
  filter(Hour == 13) |>
  ggplot(aes(max.V.MPPT, errTrinaPred)) +
  geom_boxplot(aes(group=cut_number(max.V.MPPT,5))) +
  labs(title="errTrina by max.V.MPPT, at Hour == 13")


gw.heat.hour.recent |>
  filter(Hour == 17) |>
  ggplot(aes(sd.V.MPPT, PredictedTrina.hour.kWh - Actual.hour.kWh)) +
  geom_boxplot(aes(group=cut_number(sd.V.MPPT,5))) +
  labs(title="errTrina by sd(V.MPPT), at Hour == 17")

gw.heat.hour.recent |>
  filter(Hour == 10) |>
  ggplot(aes(sd.V.MPPT, PredictedTrina.hour.kWh - Actual.hour.kWh)) +
  geom_boxplot(aes(group=cut_number(sd.V.MPPT,5))) +
  labs(title="errTrina by sd(V.MPPT), at Hour == 10")

gw.heat.hour.recent |>
  filter(Hour == 10) |>
  group_by(Date) |>
  mutate(maxV.MPPT = max(V.MPPT.1.V.)) |>
  ungroup() |>
  ggplot(aes(maxV.MPPT, PredictedTrina.hour.kWh - Actual.hour.kWh)) +
  geom_boxplot(aes(group=cut_number(maxV.MPPT,5))) +
  labs(title="errTrina by max(V.MPPT), at Hour == 10")



gw.heat.hour.recent |>
  ggplot(aes(Month, errTrinaPred)) +
  geom_boxplot(aes(group=cut_width(Month,1)))

gw.heat.hour.recent |>
  ggplot(aes(T_Amb, errTrinaPred)) +
  geom_boxplot(aes(group=cut_width(T_Amb,1)))


gw.heat.hour.recent <-
  mutate(gw.heat.hour.recent, LaggedDGPower = lag(mean.DGPower)) |>
  slice(-c(1:2)) # a hack, would be better to filter on NA in this field

lmDGTL.recent <- lm(dT.dt ~ mean.T + mean.DGPower + LaggedDGPower,
               gw.heat.hour.recent)
summary(lmDGTL.recent)
# Residuals:
# Min     1Q Median     3Q    Max
# -5.427 -1.067  0.062  1.109  5.569
#
# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)
#  (Intercept)    2.795749   0.133488   20.94   <2e-16 ***
#  mean.T        -0.160662   0.006996  -22.96   <2e-16 ***
#  mean.DGPower   0.407663   0.003368  121.04   <2e-16 ***
#  LaggedDGPower -0.283269   0.005088  -55.67   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 1.559 on 2940 degrees of freedom
# Multiple R-squared:  0.8378,	Adjusted R-squared:  0.8377
# F-statistic:  5063 on 3 and 2940 DF,  p-value: < 2.2e-16

# Interpretation: a slightly better fit than to all our data (in goodlmDG.R)
# dT.dt (in degrees Celsius/hour) = 2.8 - 0.16*InverterTemp
# + 0.408*InverterDissipation.W - 0.283*priorInverterDissipation.W
#
# For example, when the inverter at 40 degrees is (estimated) as throwing off
# 60W of power continuously, its temperature will rise by about 2.8 - 6.4
# + (0.408-0.283)(60) = 2.8 - 6.4 + 7.5 = 3.9 degrees per hour.  If it were
# throwing off 40W, the temp would still rise slightly: 2.8 - 6.4 + 5.0 = 1.4.
#
# Thermal equilibrium at 70W is (2.8 + 0.125(70))/0.16 = 72.2 degrees.
#
# An over-temperature condition might arise at 70 degrees, i.e. when the
# inverter is throwing off (70(0.16) - 2.8)/(0.125) = 67W.

summary(gw.heat.hour.recent$mean.DGPower)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.000   4.459  21.840  20.921  32.510  63.448
gwm <- max(gw.heat.hour.recent$mean.DGPower)
gwmw <- which(gw.heat.hour.recent$mean.DGPower == gwm)
print(paste0("Max DG power = ", round(gwm,1),
             " at ", gw.heat.hour.recent$dateTime[gwmw],
             ". Inverter temp = ",
             gw.heat.hour.recent$max.T[gwmw]))
summary(gw.heat.hour.recent$max.T)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.000   4.459  21.840  20.921  32.510  63.448
tm <- max(gw.heat.hour.recent$max.T)
tmw <- which(gw.heat.hour.recent$max.T == tm)
print(paste0("Max inverter temp = ", tm,
             " at ", gw.heat.hour.recent$dateTime[tm],
             ". Inverter DG power = ",
             round(gw.heat.hour.recent$mean.DGPower[tmw])))

plot(lmDGTL.recent)
# Resid v fitted shows significant nonlinearity.  Temperature drops of more
# than 5 degrees are underpredicted i.e. a prediction of -10 is actually -12.
# This nonlinearity is greater than in our full dataset, possibly suggesting
# a change in the efficiency of the Goodwe inverter as it ages, or perhaps
# just an artefact of the not-terribly representative sample of our data from
# prior years.

gw.heat.hour.recent <- gw.heat.hour.recent |>
  mutate(
    Residuals = lmDGTL.recent$residuals,
  )

gw.heat.hour.recent |> ggplot(aes(mean.V.MPPT, Residuals)) +
  geom_boxplot(aes(group=cut_width(mean.V.MPPT, 10)))
# A very strong effect, quasi-linear in mean.V.MPPT
# perhaps the effective shunt resistance has changed with age??

gw.heat.hour.recent |> ggplot(aes(errTrinaPred, Residuals)) +
  geom_boxplot(aes(group=cut_number(errTrinaPred, 5)))



gw.heat.hour.recent |> ggplot(aes(mean.V.MPPT, Residuals)) +
  geom_boxplot(aes(group=cut_width(mean.V.MPPT, 10)))
gw.heat.hour.recent |> ggplot(aes(mean.V.MPPT, Residuals)) +
  geom_boxplot(aes(group=cut_number(mean.V.MPPT, 10)))
# strong upward quasilinear trend, residual -1.5 at low V, rising to 0.5 at
# 190V, and remaining high at high-V (low-A) MPPT states.  Suggestive of
# inaccurate DG model, a lower shunt resistance, higher series resistance
# would probably decrease the regression coefficient on DGPower.

lmDGTLV.recent <- lm(dT.dt ~ mean.T + mean.DGPower + LaggedDGPower
                       + mean.V.MPPT,
                       gw.heat.hour.recent)
summary(lmDGTLV.recent)
# Rsq rises to 0.854
# coefficient of mean.V.MPPT is 0.025, i.e. +1 dT/dt when voltage rises by 40V
plot(lmDGTLV.recent) # still nonlinear at low fitted values

gwii <- mutate(gw.heat.hour.recent,
               mean.Isq = mean.I.MPPT*mean.I.MPPT)
lmDGTVII.recent <-
  lm(dT.dt ~ mean.T + mean.V.MPPT + mean.Isq, gwii)
summary(lmDGTVII.recent)

lmDGTVIIl.recent <-
  lm(dT.dt ~ mean.T + mean.V.MPPT + mean.Isq +
       lag(mean.Isq), gwii)
summary(lmDGTVIIl.recent)
plot(lmDGTVIIl.recent)


gw.heat.hour.recent |> ggplot(aes(mean.I.MPPT*mean.I.MPPT, Residuals)) +
  geom_boxplot(aes(group=cut_width(mean.I.MPPT*mean.I.MPPT, 5)))
# pretty accurate at high values of current (above 8A) and at low current, but
# midrange is nonlinear


# old code below

gw <- gw |>
  mutate(
    PeriodEnd.NZ =
      force_tz(
        date(dateTime) + hour(dateTime) * dhours(1) +
          trunc(minute(dateTime) / 30) * 30 * dminutes(1),
        tz = "NZ"
      ),
    abs.dV.MPPT.dt =
      abs((V.MPPT.1.V. - lag(V.MPPT.1.V.)) / (timeStepLag / dseconds(1))),
    abs.dPower.dt =
      abs(Power.W. - lag(Power.W.)) / (timeStepLag / dseconds(1)),
    dTemperature.dt =
      (Temperature. - lag(Temperature.)) / (timeStepLag / dseconds(1))
  ) |>
  group_by(Date) |>
  mutate(Temperature.max.day = max(Temperature.),
         Ua.V.max.day = max(Ua.V.))

gw$abs.dV.MPPT.dt[1] = 0.0
gw$abs.dPower.dt[1] = 0.0
gw$dTemperature.dt[1] = 0.0

gw_hourly <- gw |>
  group_by(Date, Hour) |>
  mutate(
    mean.hour.V.MPPT.1.V. = mean(V.MPPT.1.V.),
    mean.hour.I.MPPT.1.A. = mean(I.MPPT.1.A.),
    mean.hour.Power.W. = mean(Power.W.),
    mean.hour.Temperature. = mean(Temperature.),
    mean.hour.abs.dV.MPPT.dt = mean(abs.dV.MPPT.dt),
    mean.hour.abs.dPower.dt = mean(abs.dPower.dt),
    mean.hour.dTemperature.dt = mean(dTemperature.dt)
    ) |>
  distinct(Date,
           pick(Year : Hour,
                genHour,
                shadowScan,
                Temperature.max.day : mean.hour.dTemperature.dt)) |>
  ungroup()

#simulated Peimar 300W with geo horizon (vcc)
gw_hourlyxh <- inner_join(gw_hourly,
                         recent.peimarsim.hourly.horizon,
                         by=c("Year", "Month", "Day", "Hour")) |>
  mutate(shadowScan = factor(--shadowScan)) |>
  rename(Actual.hour.kWh = genHour) |>
  rename(PredictedPeimar.hour.kWh = E_Avail) |>
  mutate(errPred = PredictedPeimar.hour.kWh - Actual.hour.kWh) |>
  relocate(PredictedPeimar.hour.kWh : errPred, .after = Actual.hour.kWh)

summary(gw_hourlyxh$PredictedPeimar.hour.kWh)
summary(gw_hourlyxh$Actual.hour.kWh)
cor(gw_hourlyxh$PredictedPeimar.hour.kWh, gw_hourlyxh$Actual.hour.kWh)
summary(gw_hourlyxh$PredictedPeimar.hour.kWh-gw_hourlyxh$Actual.hour.kWh)
sd(gw_hourlyxh$errPred)

gw.pivot <- gw_hourlyxh |> # prep for stacked barplot
  pivot_longer(
    cols = c("errPred","Actual.hour.kWh"),
    names_to = "measure",
    values_to = "kWh"
  ) |>
  mutate(measure = forcats::fct_rev(measure))
gw.pivot |>
  ggplot(aes(fill = measure, x = Hour, y = kWh)) +
  stat_summary(fun.data = "mean_cl_boot",
               geom = "bar",
               position = "stack") +
  facet_wrap(gw.pivot$Month)

gwxh_recent_model <-
  lm(Actual.hour.kWh ~ PredictedPeimar.hour.kWh, gw_hourlyxh)
summary(gwxh_recent_model)

# monthly models
gwxh_recent_monthly_model <- gw_hourlyxh |>
  group_by(Month) |>
  nest() |>
  mutate(lm = map(data, make_modelp0)) |>
  mutate(tidy = map(lm, broom::tidy)) |>
  unnest(tidy)

#augsep 23 simulated Peimar 300W with geo horizon (vca)
gw_hourlyxh <- inner_join(gw_hourly,
                          augsep23.peimarsim.hourly.horizon,
                          by=c("Year", "Month", "Day", "Hour")) |>
  mutate(shadowScan = factor(--shadowScan)) |>
  rename(Actual.hour.kWh = genHour) |>
  rename(PredictedPeimar.hour.kWh = E_Avail) |>
  mutate(errPred = PredictedPeimar.hour.kWh - Actual.hour.kWh) |>
  relocate(PredictedPeimar.hour.kWh : errPred, .after = Actual.hour.kWh)

summary(gw_hourlyxh$PredictedPeimar.hour.kWh)
summary(gw_hourlyxh$Actual.hour.kWh)
cor(gw_hourlyxh$PredictedPeimar.hour.kWh, gw_hourlyxh$Actual.hour.kWh)
summary(gw_hourlyxh$PredictedPeimar.hour.kWh-gw_hourlyxh$Actual.hour.kWh)
sd(gw_hourlyxh$errPred)

gw.pivot <- gw_hourlyxh |> # prep for stacked barplot
  pivot_longer(
    cols = c("errPred","Actual.hour.kWh"),
    names_to = "measure",
    values_to = "kWh"
  ) |>
  mutate(measure = forcats::fct_rev(measure))
gw.pivot |>
  ggplot(aes(fill = measure, x = Hour, y = kWh)) +
  stat_summary(fun.data = "mean_cl_boot",
               geom = "bar",
               position = "stack") +
  facet_wrap(gw.pivot$Month)

gwxh_model <- lm(Actual.hour.kWh ~ PredictedPeimar.hour.kWh, gw_hourlyxh)
summary(gwxh_model)

gwxh_model <-
  lm(Actual.hour.kWh ~ PredictedPeimar.hour.kWh + shadowScan, gw_hourlyxh)
summary(gwxh_model)

#first year simulated Peimar 300W with geo horizon (vcb)
gw_hourlyxh <- inner_join(gw_hourly,
                          firstyear.peimarsim.hourly.horizon,
                          by=c("Year", "Month", "Day", "Hour")) |>
  mutate(shadowScan = factor(--shadowScan)) |>
  rename(Actual.hour.kWh = genHour) |>
  rename(PredictedPeimar.hour.kWh = E_Avail) |>
  mutate(errPred = PredictedPeimar.hour.kWh - Actual.hour.kWh) |>
  relocate(PredictedPeimar.hour.kWh : errPred, .after = Actual.hour.kWh)

summary(gw_hourlyxh$PredictedPeimar.hour.kWh)
summary(gw_hourlyxh$Actual.hour.kWh)
cor(gw_hourlyxh$PredictedPeimar.hour.kWh, gw_hourlyxh$Actual.hour.kWh)
summary(gw_hourlyxh$PredictedPeimar.hour.kWh-gw_hourlyxh$Actual.hour.kWh)
sd(gw_hourlyxh$errPred)

gw.pivot <- gw_hourlyxh |> # prep for stacked barplot
  pivot_longer(
    cols = c("errPred","Actual.hour.kWh"),
    names_to = "measure",
    values_to = "kWh"
  ) |>
  mutate(measure = forcats::fct_rev(measure))
gw.pivot |>
  ggplot(aes(fill = measure, x = Hour, y = kWh)) +
  stat_summary(fun.data = "mean_cl_boot",
               geom = "bar",
               position = "stack") +
  facet_wrap(gw.pivot$Month)

gwxh_model <- lm(Actual.hour.kWh ~ PredictedPeimar.hour.kWh, gw_hourlyxh)
summary(gwxh_model)

gwxh_first_months_model <- gw_hourlyxh |> # full models
  group_by(Month) |>
  nest() |>
  mutate(lm = map(data, make_modelp0)) |>
  mutate(tidy = map(lm, broom::tidy)) |>
  unnest(tidy)

gwxh_first_months_model2 <- gw_hourlyxh |> # summative stats of models
  group_by(Month)  |>
  do(broom::tidy(lm(Actual.hour.kWh ~ PredictedPeimar.hour.kWh + 0, .)))


# simulated Peimar 300W without horizon (vc0)
gw_hourlyx <- inner_join(gw_hourly,
                          recent.peimarsim.hourly,
                          by=c("Year", "Month", "Day", "Hour")) |>
  mutate(shadowScan = factor(--shadowScan)) |>
  rename(Actual.hour.kWh = genHour) |>
  rename(PredictedPeimar.hour.kWh = E_Avail) |>
  mutate(errPred = PredictedPeimar.hour.kWh - Actual.hour.kWh) |>
  relocate(PredictedPeimar.hour.kWh : errPred, .after = Actual.hour.kWh)

summary(gw_hourlyx$PredictedPeimar.hour.kWh)
summary(gw_hourlyx$Actual.hour.kWh)
cor(gw_hourlyx$PredictedPeimar.hour.kWh, gw_hourlyx$Actual.hour.kWh)
summary(gw_hourlyx$PredictedPeimar.hour.kWh-gw_hourlyx$Actual.hour.kWh)
sd(gw_hourlyx$errPred)

gw.pivot <- gw_hourlyx |> # prep for stacked barplot
  pivot_longer(
    cols = c("errPred","Actual.hour.kWh"),
    names_to = "measure",
    values_to = "kWh"
  ) |>
  mutate(measure = forcats::fct_rev(measure))
gw.pivot |>
  ggplot(aes(fill = measure, x = Hour, y = kWh)) +
  stat_summary(fun.data = "mean_cl_boot",
               geom = "bar",
               position = "stack") +
  facet_wrap(gw.pivot$Month)

gwx_model <- lm(Actual.hour.kWh ~ PredictedPeimar.hour.kWh, gw_hourlyx)
summary(gwx_model)

gwx_model <-
  lm(Actual.hour.kWh ~ PredictedPeimar.hour.kWh + shadowScan, gw_hourlyx)
summary(gwx_model)

# lines below haven't been adapted to current names of vars in gw

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



