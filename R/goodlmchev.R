if (!("tidyverse" %in% (.packages()))) library(tidyverse)
require(ggiraph) # interactive graphs
require(ggiraphExtra) # visualise predictions from multiple regression models

load("data/gw.rda")
load("data/gw.heat.rda")
load("data/gw.heat.day.rda")
load("data/gw.heat.hour.rda")
load("data/gw.heat.qhour.rda")



gw.heat.hourl <-
  mutate(gw.heat.hour, LaggedChevPower = lag(mean.CheveletPower)) |>
  slice(-c(1:2)) # a hack, would be better to filter on NA in this field

lmChevTL <- lm(dT.dt ~ mean.T + mean.CheveletPower + LaggedChevPower,
                gw.heat.hourl)
summary(lmChevTL)
# Interpretation: dT.dt (in degrees Celsius/hour) = 2.1 - 0.12*InverterTemp
# + 0.3*InverterDissipation.W - 0.23*priorInverterDissipation.W
#
# For example, when the inverter at 40 degrees is (estimated) as throwing off
# 60W of power continuously, its temperature will rise by about 2.1 - 4.8
# + 0.30(60) - 0.23(60) = 2.1 - 4.8 + 4.2 = 1.5 degrees per hour.  If it were
# throwing off 40W, the temp would drop slightly: 2.1 - 4.8 + 2.8 = -0.1.
#
# Thermal equilibrium at 70W is (2.1 + 0.7(70))/0.12 = 58 degrees.
#
# Thermal equilibrium at 80W is (2.1 + 5.6)/0.12 = 64 degrees.
#
# An over-temperature condition might arise at 70 degrees, i.e. when the
# inverter is throwing off (70(0.12) - 2.1)/(0.07) = 90W.

# Residuals:
# Min      1Q  Median      3Q     Max
# -9.8988 -1.1259 -0.0412  1.1374  6.9176
#
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)
# (Intercept)              2.097740   0.078534   26.71   <2e-16 ***
#  mean.T                  -0.117459   0.003997  -29.39   <2e-16 ***
#  mean.CheveletPower       0.295969   0.001611  183.68   <2e-16 ***
#  lag(mean.CheveletPower) -0.226295   0.002192 -103.22   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 1.689 on 8437 degrees of freedom
# (2 observations deleted due to missingness)
# Multiple R-squared:  0.8039,	Adjusted R-squared:  0.8039
# F-statistic: 1.153e+04 on 3 and 8437 DF,  p-value: < 2.2e-16
plot(lmChevTL)

# Adding mean.H.Total to the regression is a very slight improvement, maybe just
# collinearity at work! Note that the coefficient on mean.H.total has a very
# small value, just 0.4 degrees of temp rise for every 10k hours.  So we'll
# investigate the simpler model first.
lmChevTHL <- lm(dT.dt ~ mean.T + mean.CheveletPower + mean.H.Total +
                    lag(mean.CheveletPower),
                  gw.heat.hour)
summary(lmChevTHL)
# Residuals:
# Min      1Q  Median      3Q     Max
#  -9.3112 -1.1183 -0.0177  1.1038  6.8344
#
# Coefficients:
#                          Estimate   Std. Error  t value  Pr(>|t|)
#  (Intercept)              2.153e+00  7.778e-02   27.68   <2e-16 ***
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

# Returning to the simpler model...
gw.heat.hourl <- gw.heat.hourl |>
  mutate(
    Residuals = lmChevTL$residuals,
    QYear = factor(quarter(ymd(Date), type = "year.quarter"))
    )
gw.heat.hourl |> ggplot(aes(Hour, Residuals)) +
  geom_boxplot(aes(group=cut_width(Hour, 1)))
gw.heat.hourl |> ggplot(aes(Month, Residuals)) +
  geom_boxplot(aes(group=cut_width(Month, 1)))
gw.heat.hourl |> ggplot(aes(QYear, Residuals)) +
  geom_boxplot(aes(group=cut_width(QYear, 1)))
gw.heat.hourl |> ggplot(aes(mean.T, Residuals)) +
  geom_boxplot(aes(group=cut_width(mean.T, 5)))
gw.heat.hourl |> ggplot(aes(Hour-first.qhour.day, Residuals)) +
  geom_boxplot(aes(group=cut_width(Hour-first.qhour.day, 1)))
# Mean residuals are +ve during the first 6h of daily generation, -ve during
# the next 5h, then +ve for the last few hours.  Maybe 20% to 40% of the
# unexplained variance?
gw.heat.hourl |> ggplot(aes(mean.V.MPPT, Residuals)) +
  geom_boxplot(aes(group=cut_width(mean.V.MPPT, 10)))
# a strong effect here, with -ve residual at voltage below 150V and +ve
# above 190V.  This will be a limitation of the Chevelet model, as it isn't
# sensitive to the nonlinearity of the panels.
gw.heat.hourl |> ggplot(aes(mean.I.MPPT, Residuals)) +
  geom_boxplot(aes(group=cut_width(mean.I.MPPT, 1)))
gw.heat.hourl |> ggplot(aes(mean.I.MPPT, Residuals)) +
  geom_boxplot(aes(group=cut_number(mean.I.MPPT, 7)))
# Our modelling is inaccurate when I.MPPT < 0.5, the inverter apparently throws
# off more heat than expected.  There are many such datapoints, perhaps 30%, so
# they'll be significantly affecting the regression.  Perhaps: fit to the
# higher-current data?  Note: we may be underpredicting slightly at high
# currents (above 6A) but a 0 residual is within the semi-interquartile range.
gw.heat.hourl |> ggplot(aes(mean.I.MPPT*mean.I.MPPT, Residuals)) +
  geom_boxplot(aes(group=cut_width(mean.I.MPPT*mean.I.MPPT, 5)))
# Residual is slightly -ve for highest current flows (above 9A), and also for
# lowest current flows(below 4A), so perhaps we'd do better with a slightly
# lower value for series resistance and a slightly higher value for shunt
# resistance in our Chevelet model of the Goodwe 3000-NS?

gw.heat.hourl |> ggplot(aes(sd.V.MPPT, Residuals)) +
  geom_boxplot(aes(group=cut_number(sd.V.MPPT, 10)))
# High sd on V.MPPT doesn't greatly affect the residuals. We're underpredicting
# slightly when sd is the lowest decile.


plot(lmChevTL) # high residuals at fitted values below -5
# apparent outliers at 171, 6, 2842, 4642

outl <- 171
print(
  paste(
    "Apparent outlier: residual =",
    round(gw.heat.hourl$Residuals[outl], 2),
    "at",
    gw.heat.hourl$dateTime[outl],
    "in",
    gw.heat.hourl$Source[outl]
  )
)
gwx1 <- filter(gw.heat.hourl, Date == gw.heat.hourl$Date[outl])
# Generation started at noon: an anomalous day for inverter heating

outl <- 6
print(
  paste(
    "Apparent outlier: residual =",
    round(gw.heat.hourl$Residuals[outl], 2),
    "at",
    gw.heat.hourl$dateTime[outl],
    "in",
    gw.heat.hourl$Source[outl]
  )
)
gwx2 <- filter(gw.heat.hourl, Date == gw.heat.hourl$Date[outl])
# generation started at 11am

outl <- 2842
print(
  paste(
    "Apparent outlier: residual =",
    round(gw.heat.hourl$Residuals[outl], 2),
    "at",
    gw.heat.hourl$dateTime[outl],
    "in",
    gw.heat.hourl$Source[outl]
  )
)
gwx3 <- filter(gw.heat.hourl, Date == gw.heat.hourl$Date[outl])
# The first significant generation of the day.  ChevEnergy was 71Wh, genHour
# was 1.22kWh.  This hour's temp rise was underpredicted, and the following
# hour was overpredicted.

outl <- 4642
print(
  paste(
    "Apparent outlier: residual =",
    round(gw.heat.hourl$Residuals[outl], 2),
    "at",
    gw.heat.hourl$dateTime[outl],
    "in",
    gw.heat.hourl$Source[outl]
  )
)
gwx4 <- filter(gw.heat.hourl, Date == gw.heat.hourl$Date[outl])
# Looks like the temp rise in this hour was increased by heat flux from the
# prior hour, and thermal equilibrium was nearly reached this hour.  To model
# accurately, I think it'd be necessary to estimate a thermal differential
# within the inverter.


# regressing on an accurate 1-hour lag of ChevPower gives marginally better
# results for Rsq but not for residual std error, and we're "throwing away"
# about 10% of the data.  I'll stick with the simpler model.
gw.heat.hourl1 <-
  filter(gw.heat.hour,
         !is.na(lag1H.Chev.Power + mean.CheveletPower + mean.T + dT.dt))
lmChevTL1 <- lm(dT.dt ~ mean.T + mean.CheveletPower + lag1H.Chev.Power,
                gw.heat.hourl1)
summary(lmChevTL1)
# Residuals:
# Min      1Q  Median      3Q     Max
# -9.8268 -1.1180 -0.0017  1.1361  7.0414
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#  (Intercept)         2.795628   0.088745   31.50   <2e-16 ***
#  mean.T             -0.140812   0.004305  -32.71   <2e-16 ***
#  mean.CheveletPower  0.293829   0.001649  178.15   <2e-16 ***
#  lag1H.Chev.Power   -0.220797   0.002276  -97.01   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 1.715 on 7783 degrees of freedom
# Multiple R-squared:  0.8114,	Adjusted R-squared:  0.8113
# F-statistic: 1.116e+04 on 3 and 7783 DF,  p-value: < 2.2e-16

# try regressing on qhour data
gw.heat.qhourl <-
  mutate(gw.heat.qhour, LaggedChevPower = lag(mean.CheveletPower)) |>
  slice(-c(1:2)) # a hack, would be better to filter on NA in this field

lmChevQTL <- lm(dT.dt ~ mean.T + mean.CheveletPower + LaggedChevPower,
               gw.heat.qhourl)
summary(lmChevQTL)
# Rsq is 0.60, std error 3.2.  Much less predictive.


# old lms
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





