#

if (!("tidyverse" %in% (.packages()))) library(tidyverse)
require(ggiraph) # interactive graphs
require(ggiraphExtra) # visualise predictions from multiple regression models

load("data/gw.rda")
load("data/gw.heat.rda")
load("data/gw.heat.day.rda")
load("data/gw.heat.hour.rda")
load("data/gw.heat.qhour.rda")

lmGenT <- lm(dT.dt ~ mean.T + genHour,
              gw.heat.hour)
summary(lmGenT) # Rsq = 0.5427
plot(lmGenT)
# unclear whether CheveletPower is a better predictor of heat than genHour!
lmGenTH <- lm(dT.dt ~ mean.T + genHour + mean.H.Total,
             gw.heat.hour)
summary(lmGenTH) # Rsq = 0.587
plot(lmGenTH) # nasty looking linear cluster of points on Residuals v Leverage

lmChevT <- lm(dT.dt ~ mean.T + mean.CheveletPower,
                  gw.heat.hour)
summary(lmChevT)
# Residuals:
#      Min       1Q   Median       3Q      Max
# -11.1023  -1.7691   0.1908   1.7502  10.7316
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
# (Intercept)         6.650e+00  9.773e-02   68.04   <2e-16 ***
#  mean.T             -3.988e-01  4.396e-03  -90.72   <2e-16 ***
#  mean.CheveletPower  2.045e-07  2.024e-09  101.03   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 2.54 on 8439 degrees of freedom
# (1 observation deleted due to missingness)
# Multiple R-squared:  0.5564,	Adjusted R-squared:  0.5563
# F-statistic:  5292 on 2 and 8439 DF,  p-value: < 2.2e-16

plot(lmChevT)
# Apparent outlier: residual = -11.07 at 2018-11-16 14:00:00
# in data-raw/xls-withoutSS/Historical Data Export-20231027155612.xls
# highly variable clouds, two 5-min periods at 11A (1800W), others at 300W,
# temp dropped from 44.2 to 34.5.  Mean power 690W, ChevEnergy 45Wh.  On
# trendline of QQ, so this misprediction is of some "normal" type.
outl <- 2546
cat(paste("Apparent outlier: residual =", round(lmChevTH$residuals[outl],2),
    "at", gw.heat.hour$dateTime[outl], "in", gw.heat.hour$Source[outl]))
# Apparent outlier: residual = 10.77 at 2022-01-20 14:00:00
# in data-raw/xls-withoutSS/Historical Data Export-20231027154851.xls
# Variable clouds, power from 586W to 1813W (12A).  Temp rose from 32.3 to
# 45.4.  ChevEnergy 58W, mean power 973W.  On trendline of QQ.
outl2 <- 4438
cat(paste("Apparent outlier: residual =", round(lmChevTH$residuals[outl2],2),
          "at", gw.heat.hour$dateTime[outl2], "in", gw.heat.hour$Source[outl2]))
# Apparent outlier: residual = 9.98 at 2018-04-10 14:00:00
# in data-raw/xls-dmy/firstweeks.rev.xlsx
# Our first datapoint.  Starting temp was 16.3, ending 30.2; with mean power
# 774W, ChevEnergy 41W.  One of only 3 points at 4x mean residual.  Not much
# leverage so we'll leave it alone... but maybe another reason to exclude
# the first hour of every day?
#
outl3 <- 1
cat(paste("Apparent outlier: residual =", round(lmChevTH$residuals[outl3],2),
          "at", gw.heat.hour$dateTime[outl3], "in", gw.heat.hour$Source[outl3]))

lmChevTH <- lm(dT.dt ~ mean.T + mean.CheveletPower + mean.H.Total,
              gw.heat.hour)
summary(lmChevTH)
# Residuals:
# Min      1Q  Median      3Q     Max
# -10.2555  -1.6591   0.1334   1.6864  10.2954
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
# (Intercept)         6.288e+00  9.386e-02   67.00   <2e-16 ***
#  mean.T             -4.350e-01  4.361e-03  -99.74   <2e-16 ***
#  mean.CheveletPower  2.183e-07  1.983e-09  110.08   <2e-16 ***
#  mean.H.Total        1.220e-04  4.135e-06   29.51   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 2.418 on 8438 degrees of freedom
# (1 observation deleted due to missingness)
# Multiple R-squared:  0.5979,	Adjusted R-squared:  0.5977
# F-statistic:  4181 on 3 and 8438 DF,  p-value: < 2.2e-16

# First T of the day is a proxy for ambient temperature all day, and anyway
# it's important for the first dT.dt
lmChevTTH <- lm(dT.dt ~ mean.T + mean.CheveletPower + mean.H.Total +
                  first.T.day,
               gw.heat.hour)
summary(lmChevTTH)
# Residuals:
# Min       1Q   Median       3Q      Max
# -9.7867 -1.4942  0.0413  1.5781  8.4426
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
# (Intercept)         4.100e+00  9.683e-02   42.35   <2e-16 ***
#  mean.T             -5.371e-01  4.505e-03 -119.24   <2e-16 ***
#  mean.CheveletPower  2.480e-07  1.892e-09  131.05   <2e-16 ***
#  mean.H.Total        1.068e-04  3.719e-06   28.72   <2e-16 ***
#  first.T.day         3.027e-01  6.643e-03   45.57   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 2.166 on 8437 degrees of freedom
# (1 observation deleted due to missingness)
# Multiple R-squared:  0.6773,	Adjusted R-squared:  0.6771
# F-statistic:  4427 on 4 and 8437 DF,  p-value: < 2.2e-16
plot(lmChevTTH) # nonlinear; underpredicting at low and high fitted values
# QQ is ok in middle but not at extremes
# only a few dozen points with high leverage,
# 2986 is identified on leverage plot, residual about +2s.d.
# 2546 has residual +2sd, fitted to about 0.0

outl <- 2546
cat(paste("Apparent outlier: residual =", round(lmChevTTH$residuals[outl],2),
          "at", gw.heat.hour$dateTime[outl], "in", gw.heat.hour$Source[outl]))
gwx <- filter(gw.heat.hour, Date==gw.heat.hour$Date[outl])

gw.heat.hourm1 <- slice(gw.heat.hour,-1)
lmChevTTHm1 <- lm(dT.dt ~ mean.T + mean.CheveletPower + mean.H.Total +
                  first.T.day,
                gw.heat.hourm1)
summary(lmChevTTHm1)
gw.heat.hourm1 <- bind_cols(gw.heat.hourm1, lmChevTTHm1$residuals) |>
  rename(Residuals = ...32)

gw.heat.hourm1 |> ggplot(aes(Hour, Residuals)) +
  geom_boxplot(aes(group=cut_width(Hour, 1)))

lmChevTHL <- lm(dT.dt ~ mean.T + mean.CheveletPower + mean.H.Total +
                    lag(mean.CheveletPower),
                  gw.heat.hour)
summary(lmChevTHL)
#Residuals:
# Min      1Q  Median      3Q     Max
#  -9.3112 -1.1183 -0.0177  1.1038  6.8344
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)              2.153e+00  7.778e-02   27.68   <2e-16 ***
#  mean.T                  -1.405e-01  4.295e-03  -32.72   <2e-16 ***
#  mean.CheveletPower       2.970e-07  1.596e-09  186.16   <2e-16 ***
#  mean.H.Total             4.089e-05  2.978e-06   13.73   <2e-16 ***
#  lag(mean.CheveletPower) -2.175e-07  2.261e-09  -96.20   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 1.67 on 8436 degrees of freedom
# (2 observations deleted due to missingness)
# Multiple R-squared:  0.8082,	Adjusted R-squared:  0.8081
# F-statistic:  8888 on 4 and 8436 DF,  p-value: < 2.2e-16

plot(lmChevTHL) # high residuals at fitted values below -5
# apparent outliers at 173, 8, 2844, 4644


gw.heat.hourx <-
  slice(gw.heat.hour,-lmChevTHL$na.action) |>
  mutate(gw.heat.hourm2, Residuals = lmChevTHL$residuals)
gw.heat.hourx |> ggplot(aes(Hour, Residuals)) +
  geom_boxplot(aes(group=cut_width(Hour, 1)))


gw.heat.hour.after9 <- filter(gw.heat.hourm1, Hour>9)
lmChevTHa9 <- lm(dT.dt ~ mean.T + mean.CheveletPower + mean.H.Total,
                  gw.heat.hour.after9)
summary(lmChevTHa9)
gw.heat.hour.after9 <- bind_cols(gw.heat.hour.after9, lmChevTHa9$residuals) |>
  rename(ResidualsA9 = ...33)
gw.heat.hour.after9 |> ggplot(aes(Hour, ResidualsA9)) +
  geom_boxplot(aes(group=cut_width(Hour, 1)))

gw.heat.hour.a9b5 <- filter(gw.heat.hourm1, Hour>9 & Hour < 17)
lmChevTHa9b5 <- lm(dT.dt ~ mean.T + mean.CheveletPower + mean.H.Total,
                 gw.heat.hour.a9b5)
summary(lmChevTHa9b5)
gw.heat.hour.a9b5 <- bind_cols(gw.heat.hour.a9b5, lmChevTHa9b5$residuals) |>
  rename(ResidualsA9b5 = ...33)
gw.heat.hour.a9b5 |> ggplot(aes(Hour, ResidualsA9b5)) +
  geom_boxplot(aes(group=cut_width(Hour, 1)))
plot(lmChevTHa9b5)

# Rsq 0.496
lmChevQT <- lm(dT.dt ~ mean.T + mean.CheveletPower,
               gw.heat.qhour)
summary(lmChevQT)
plot(lmChevQT)
# serious nonlinearity in QQ

# Rsq 0.2124
lmChevAT <- lm(dT.dt ~ Temperature. + CheveletPower,
               gw.heat)
summary(lmChevAT)
plot(lmChevAT)
# v. serious nonlinearity in QQ.  Maybe need a lagged CheveletPower?

# Rsq 0.2016, lagging doesn't help
# The temperature sensor must be very close to the SCR!
lmChevLAT <- lm(dT.dt ~ Temperature. + lag(CheveletPower),
               gw.heat)
summary(lmChevLAT)


# ChevEnergy stacked with netGenHour

gw.pivot <- gw.heat.hour |>
  pivot_longer(
    cols = c("netGenHour", "CheveletEnergy"),
    names_to = "type",
    values_to = "kWh"
  )

gw.pivot |>
  ggplot(aes(fill = type, x = Hour, y = kWh)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "bar", position = "stack")

gw.pivot |>
  ggplot(aes(fill = type, x = Hour, y = kWh)) +
  stat_summary(fun.data = "mean_cl_boot",
               geom = "bar",
               position = "stack") +
  facet_wrap(month(as.Date(gw.pivot$Date, tz="NZ"), label=TRUE, abbr=TRUE))

# max inverter temp by month and year
ggplot(gw.heat.day,
       aes(x=Month, y=max.T, colour=Year)) +
  geom_boxplot()

# daily generation by month and year
ggplot(gw.heat.day,
       aes(x=Month, y=genDay, colour=Year)) +
  geom_boxplot()

summary(gw.heat.qhour$dT.dt)
gw.heat.qhour$dateTime[which.max(gw.heat.qhour$dT.dt)]

ggplot(gw.heat.qhour, aes(x=dT.dt, y=genQHour, colour=Year)) +
  geom_point()

# Modelling the temperature rise in our Goodwe NS-3000 inverter.


#
# (The comments below were made prior to a round of data cleaning.  Rsq is
# currently on 0.5018, suggesting that my adjustments to the calculation
# of dt after a Wait or Fault have degraded the accuracy of dT.dt, or that
# genHour now has some defects.)

# Most simply: it is proportional to its output power, and inversely
# proportional to its current temperature (because it is convection-cooled).
#
# Note: an hourly average power in kW is equal to the hourly kWh.  I use
# genHour because I have validated it (roughly) against other statistics;
# mean.Power.W is distributed slightly differently.
#
# Best fit is dT/dt = 10.5 + 11.3(+/-0.1)kW - 0.462(+/- 0.004)T,
# in degrees C per hour; Rsq = 0.61
#
# For example, at 1.5kW (i.e. when genHour = 1.5kWh), an inverter at 40 degrees
# will have dT/dt = 10.5 + 11.3*1.5 - 0.462*40 = 17.0 - 18.5 = 8.5.   Thermal
# equilibrium would be at about 10.5 + 17.0/0.462 = 47.3 degrees C, according
# to this not very highly-predictive model.
#
# The intercept of 10.5 seems rather high, but then again if we normalise
# T to a difference from a nominal room temperature of 20 degrees C, the
# intercept is very close to zero -- as one might expect, because an
# inverter that idles overnight would presumably be at thermal equilibrium.
# The ambient temperature in my hallway will vary by maybe 5 degrees seasonally,
# suggesting a seasonal error of (+/- 3)*0.462 degrees/hour.
temp_model <- lm(dT.dt ~ genHour + mean.T,
                 gw.heat.hour)
summary(temp_model)
plot(temp_model)
# Residuals:
# Min       1Q   Median       3Q      Max
# -10.1378  -1.5836  -0.0165   1.7628  21.4421
#
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
#  (Intercept) 10.471541   0.103374   101.3   <2e-16 ***
#  genHour     11.315059   0.105511   107.2   <2e-16 ***
#  mean.T      -0.462798   0.004269  -108.4   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 2.454 on 8405 degrees of freedom
# (35 observations deleted due to missingness)
# Multiple R-squared:  0.6088,	Adjusted R-squared:  0.6087
# F-statistic:  6540 on 2 and 8405 DF,  p-value: < 2.2e-16
ggPredict(temp_model,se=TRUE,interactive=TRUE)

# model using mean Power as reported by Goodwe
temp_modelW <- lm(dT.dt ~ mean.Power.W + mean.T,
                 gw.heat.hour)
summary(temp_modelW)
# (almost exactly the same fit, and is still Rsq=0.6114 after data cleaning

# It seems very likely that our inverter is becoming less efficient over time.
# Most simply, if we assume the additional heat is proportional to its total
# hours of operation:

temp_modela <- lm(dT.dt ~ genHour + mean.T + mean.H.Total,
                  gw.heat.hour)
summary(temp_modela)
# Residuals:
#  Min      1Q    Median   3Q     Max
# -9.5786 -1.4713  0.0152  1.5688 22.3817
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#  (Intercept)   1.020e+01  9.669e-02  105.54   <2e-16 ***
#  genHour       1.220e+01  1.015e-01  120.20   <2e-16 ***
#  mean.T       -5.051e-01  4.156e-03 -121.56   <2e-16 ***
#  mean.H.Total  1.396e-04  3.929e-06   35.53   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 2.289 on 8404 degrees of freedom
# (35 observations deleted due to missingness)
# Multiple R-squared:  0.6599,	Adjusted R-squared:  0.6598
# F-statistic:  5435 on 3 and 8404 DF,  p-value: < 2.2e-16

summary(gw.heat.hour$mean.H.Total)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 0.184  1369.833 11724.000  8585.584 14676.500 16795.000
summary(gw.heat.hour$genHour)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.00000 0.01581 0.23098 0.37799 0.66327 1.51226

# Interpretation:
#
# best-fit dT/dt = 12.2(+/- 0.1)kW - 0.505(+/- 0.004)T + 0.140(+/- 0.4)kh
# in degrees C per hour; Rsq = 0.66
#
# For example, an inverter at 40 degrees that's producing 1.5kW, and which
# has logged 10000 hours of operation, will have dT/dt = 12.2*1.5 -
# 0.505*40 + 0.140*10 = 18.3 - 20.2 + 1.4 = -0.5.  Thermal equilibrium at 1.5kW
# is roughly (18.3+1.4)/.505 = 39.0 degrees for this aging inverter (about 3yo:
# my Goodwe is running about 3000 hours/year).
#
# At 20000 hours, equilibrium is projected to be (18.3+2.8)/.505 = 41.8 degrees.
# Thermal cut-out seems quite remote, as my system rarely produces more than
# 1.5kW.
#
# Since every kW of output power produces (roughly) a 1/12.2 deg/h rise in its
# temperature, the additional 2.8 deg/h rise in temperature due to aging (after
# about 6 years of operation: 20kh) is 2.8/12.2 = 230W of increased "overhead"
# -- for an inverter which started its life (according to my Chevelet model-fit)
# with an equivalent series resistance of about 0.5 ohms, and an equivalent
# parallel resistance of about 1600 ohms.  At 170V 9A (i.e. about 1.53kW of
# power from the array), (170*170)/1600 = 18W and 9*.5*.5 = 2.25W = 20W. To
# generate 230W of additional heat from a 9A current, the equivalent series
# resistance would have to rise to approx $\sqrt(230/9) = 5 ohms... which is
# unbelievable.  Alternatively, the equivalent parallel resistance (internal
# consumption) could drop to approx (170*170)/230 = 126 ohms... which also
# seems unbelievable.  I think it *much* more likely that the aging process
# will affect the high-amperage efficiency of the inverter, perhaps by
# passivation of the ScR junction... and will do some modelling below under
# this assumption.

# as expected, an aging inverter is less efficient at higher power
#
# (Data cleaning and adjustment of dt have lowered Rsq to 0.6386.  A poor fit
# at high fitted values.)
temp_modelb <- lm(dT.dt ~ genHour + mean.T + mean.H.Total*genHour,
                  gw.heat.hour)
summary(temp_modelb)
#
# Residuals:
# Min      1Q  Median      3Q     Max
#-8.3795 -1.4104 -0.0041  1.5053 21.6766
#
# Coefficients:
#  Estimate Std. Error  t value Pr(>|t|)
#  (Intercept)           1.161e+01  1.001e-01  116.002  < 2e-16 ***
#  genHour               1.003e+01  1.153e-01   87.027  < 2e-16 ***
#  mean.T               -5.279e-01  3.963e-03 -133.205  < 2e-16 ***
#  mean.H.Total          3.596e-05  4.820e-06    7.461 9.48e-14 ***
#  genHour:mean.H.Total  3.116e-04  9.319e-06   33.436  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 2.15 on 8403 degrees of freedom
# (35 observations deleted due to missingness)
# Multiple R-squared:  0.6998,	Adjusted R-squared:  0.6997
# F-statistic:  4897 on 4 and 8403 DF,  p-value: < 2.2e-16

plot(temp_modelb) # outliers at 436, 3612, maybe 1, 2997

gw.heat.hour$Source[3612] # "...20231027155748.xls"
gw.heat.hour$dateTime[3612] # "2019-01-27 13:00:00 NZDT"
# This data is logged at 5min intervals, so the estimators I hacked for 1min
# deltas are probably biased.  More importantly, there is a 45min gap from
# 2019.01.27 12:45:36 to 2019.01.27 13:28:32 during which the inverter temp
# rose from 39.1 to 41.1, and total generation went up by 0.7kWh, strongly
# suggesting a log-retention defect.  I have interpolated the apparently-
# missing data and rebuilt the gw datasets.

gw.heat.hour$Source[436] # "data-raw/xls-dmy/firstweeks.xlsx"
gw.heat.hour$dateTime[436] # "2018-05-24 13:00:00 NZST"
# no generation from 1:09 to 2:08, apparently a dark rainy day, many stop/starts
# of the MPPT.  A break in the clouds (1143W for 1 min) at 1:09 skewed the
# average MPPT power during the 9 minutes the panels were producing, and
# it seems I hugely mis-estimated genHour as well as CheveletPower (see below).
# There's a Wait logged at 5/24/2018 2:07:08 PM, and temp went down from 22.1 to
# 19.5, strongly suggesting that the panels were not producing during this
# period.

gw.heat.hour$Source[1] # "data-raw/xls-dmy/firstweeks.xlsx"
gw.heat.hour$dateTime[1] # "2018-04-10 14:00:00 NZST"
# my first datapoint should have a dT.dt == NA rather than 0!

gw.heat.hour$Source[2997] # "...20231027155649.xls"
gw.heat.hour$dateTime[2997] # "2018-12-17 14:00:00 NZDT"
# very strong generation through this period, temp rising from 40.5 to 48.7
# data is logged at 5min intervals so maybe this is skewing my hourly estimates?


# An aging inverter will be less efficient at higher amperage
temp_modelc <- lm(dT.dt ~ genHour + mean.T + mean.H.Total*mean.I.MPPT,
                  gw.heat.hour)
summary(temp_modelc)
# Residuals:
# Min      1Q  Median      3Q     Max
# -8.5013 -1.3883  0.0196  1.4853 20.7412
#
# Coefficients:
#  Estimate Std. Error  t value Pr(>|t|)
#  (Intercept)               1.185e+01  9.959e-02  118.978  < 2e-16 ***
#  genHour                   4.047e+00  5.157e-01    7.848 4.75e-15 ***
#  mean.T                   -5.426e-01  3.987e-03 -136.101  < 2e-16 ***
#  mean.H.Total              3.974e-05  4.791e-06    8.294  < 2e-16 ***
#  mean.I.MPPT               1.013e+00  8.567e-02   11.823  < 2e-16 ***
#  mean.H.Total:mean.I.MPPT  4.884e-05  1.484e-06   32.921  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 2.114 on 8402 degrees of freedom
# (35 observations deleted due to missingness)
# Multiple R-squared:  0.7098,	Adjusted R-squared:  0.7096
# F-statistic:  4110 on 5 and 8402 DF,  p-value: < 2.2e-16

# Increased Joule heating (I^2R) in the SCR will be proporational to the square
# of the amperage multiplied by the hours of operation, if we assume that the
# SCR's junction resistance increases linearly with hours.  However this doesn't
# explain much of the unexplained variance.
plot(temp_modelc)
# definitely some dirtiness at 3612, 436; with 3612 having a high leverage

temp_modeld <- lm(dT.dt ~ genHour + mean.T
                  + mean.H.Total*mean.I.MPPT*mean.I.MPPT,
                  gw.heat.hour)
summary(temp_modeld)
# Residuals:
# Min      1Q  Median      3Q     Max
# -8.5013 -1.3883  0.0196  1.4853 20.7412
#
# Coefficients:
#                            Estimate Std. Error  t value Pr(>|t|)
#  (Intercept)               1.185e+01  9.959e-02  118.978  < 2e-16 ***
#  genHour                   4.047e+00  5.157e-01    7.848 4.75e-15 ***
#  mean.T                   -5.426e-01  3.987e-03 -136.101  < 2e-16 ***
#  mean.H.Total              3.974e-05  4.791e-06    8.294  < 2e-16 ***
#  mean.I.MPPT               1.013e+00  8.567e-02   11.823  < 2e-16 ***
#  mean.H.Total:mean.I.MPPT  4.884e-05  1.484e-06   32.921  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 2.114 on 8402 degrees of freedom
# (35 observations deleted due to missingness)
# Multiple R-squared:  0.7098,	Adjusted R-squared:  0.7096
# F-statistic:  4110 on 5 and 8402 DF,  p-value: < 2.2e-16

# The MPPT voltage is somewhat variable and including this in the regression
# improves its prediction slightly, to 0.7129.  However this is a highly
# nonlinear model with several collinear factors, so I'd first investigate
# the simplest model with R about 0.70, i.e. tempmodelb.
temp_modele <- lm(dT.dt ~ genHour + mean.T
                  + mean.H.Total*mean.I.MPPT*genHour,
                  gw.heat.hour)
summary(temp_modele)
# Residuals:
# Min      1Q  Median      3Q     Max
#-8.9189 -1.3705  0.0355  1.4716 20.3304

#Coefficients:
#  Estimate Std. Error  t value Pr(>|t|)
# (Intercept)                       1.165e+01  1.031e-01  113.021  < 2e-16 ***
#  genHour                           3.360e+00  7.785e-01    4.317  1.6e-05 ***
#  mean.T                           -5.471e-01  4.003e-03 -136.679  < 2e-16 ***
#  mean.H.Total                      5.379e-05  5.693e-06    9.449  < 2e-16 ***
#  mean.I.MPPT                       1.535e+00  1.402e-01   10.943  < 2e-16 ***
#  mean.H.Total:mean.I.MPPT          3.475e-05  1.405e-05    2.472 0.013438 *
#  genHour:mean.H.Total             -2.356e-05  7.794e-05   -0.302 0.762416
#  genHour:mean.I.MPPT              -3.500e-01  4.088e-02   -8.563  < 2e-16 ***
#  genHour:mean.H.Total:mean.I.MPPT  1.576e-05  4.091e-06    3.853 0.000118 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 2.103 on 8399 degrees of freedom
# (35 observations deleted due to missingness)
# Multiple R-squared:  0.7129,	Adjusted R-squared:  0.7127
# F-statistic:  2607 on 8 and 8399 DF,  p-value: < 2.2e-16

# CheveletEnergy has a complex formula, highly nonlinear

# ouch there's a *terrible* misprediction here!
temp_modelg <- lm(dT.dt ~ Temperature. + CheveletPower,
                  gw.heat)
summary(temp_modelg)
# Residuals:
# Min       1Q   Median       3Q      Max
# -1078.73    -3.17     1.54     6.41   637.82
# Multiple R-squared:  0.02576,	Adjusted R-squared:  0.02575

temp_modelh <- lm(dT.dt ~ mean.T + mean.CheveletPower + mean.H.Total,
                  gw.heat.hour)
summary(temp_modelh)
plot(temp_modelh)

cat(paste("Outlier at", gw.heat.hour$dateTime[436],
          "from", gw.heat.hour$Source[436]))
cat(paste("Outlier at", gw.heat.hour$dateTime[6469],
          "from", gw.heat.hour$Source[6469]))


# quarter-hour models have lower R-sq, due to discretisation noise and also
# maybe from heat taking many minutes to spread?
temp_model.qha <- lm(dT.dt ~ genQHour + mean.T + mean.H.Total,
                     gw.heat.qhour)
summary(temp_model.qha)
temp_model.qhb <- lm(dT.dt ~ genQHour + mean.T + mean.H.Total*genQHour,
                     gw.heat.qhour)
summary(temp_model.qhb)
temp_model.qhc <- lm(dT.dt ~ genQHour + mean.T + mean.H.Total*mean.I.MPPT,
                     gw.heat.qhour)
summary(temp_model.qhc)


gw.heat.hour.hightemp <- filter(gw.heat.hour, mean.T > 40)
hightemp_model <- lm(dT.dt ~ genHour + mean.T,
                     gw.heat.hour.hightemp)
summary(hightemp_model)
ggPredict(hightemp_model,se=TRUE,interactive=TRUE)

# other plots




