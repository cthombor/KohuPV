if (!("tidyverse" %in% (.packages()))) library(tidyverse)
require(ggiraph) # interactive graphs
require(ggiraphExtra) # visualise predictions from multiple regression models

load("data/gw.rda")
load("data/gw.heat.rda")
load("data/gw.heat.day.rda")
load("data/gw.heat.hour.rda")
load("data/gw.heat.qhour.rda")

# our original DG params: RP = 0.5, RS = 1600
gw.heat.hourl0 <-
  mutate(gw.heat.hour, LaggedDGPower.0 = lag(mean.DGPower.0)) |>
  slice(-c(1:2)) # a hack, would be better to filter on NA in this field

lmDGTL0 <- lm(dT.dt ~ mean.T + mean.DGPower.0 + LaggedDGPower.0,
             gw.heat.hourl0)
summary(lmDGTL0)
# Residuals:
# Min       1Q   Median       3Q      Max
# -11.2711  -1.1052  -0.0259   1.1300   6.7311
#
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)
# (Intercept)      2.215626   0.076503   28.96   <2e-16 ***
# mean.T          -0.123621   0.003883  -31.84   <2e-16 ***
# mean.DGPower.0   0.305365   0.001543  197.87   <2e-16 ***
# LaggedDGPower.0 -0.231579   0.002133 -108.59   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 1.664 on 9126 degrees of freedom
# Multiple R-squared:  0.8147,	Adjusted R-squared:  0.8146
# F-statistic: 1.337e+04 on 3 and 9126 DF,  p-value: < 2.2e-16

gw.heat.hourl <-
  mutate(gw.heat.hour, LaggedDGPower = lag(mean.DGPower)) |>
  slice(-c(1:2)) # a hack, would be better to filter on NA in this field

lmDGTL <- lm(dT.dt ~ mean.T + mean.DGPower + LaggedDGPower,
                gw.heat.hourl)
summary(lmDGTL)
# Residuals:
# Min      1Q  Median      3Q     Max
# -9.7059 -1.0939 -0.0316  1.1232  6.7682
#
# Coefficients:
#                 Estimate   Std. Error t value Pr(>|t|)
#  (Intercept)    2.156993   0.077693   27.76   <2e-16 ***
#  mean.T        -0.119441   0.003941  -30.30   <2e-16 ***
#  mean.DGPower   0.301857   0.001598  188.85   <2e-16 ***
#  LaggedDGPower -0.230624   0.002180 -105.81   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 1.651 on 8437 degrees of freedom
# Multiple R-squared:  0.8125,	Adjusted R-squared:  0.8124
# F-statistic: 1.219e+04 on 3 and 8437 DF,  p-value: < 2.2e-16

# Interpretation: dT.dt (in degrees Celsius/hour) = 2.16 - 0.119*InverterTemp +
# 0.302*InverterDissipation.W - 0.231*priorInverterDissipation.W
#
# For example, when the inverter at 40 degrees is (estimated) as throwing off
# 60W of power continuously, its temperature will rise by about 2.16 -
# (0.302 - 0.231)(60) = 2.08 degrees per hour.  If it were dissipating 40W,
# its temperature would drop slowly: 2.16 - (0.071)(40) = -0.68 degrees/h.
#
# Thermal equilibrium at 70W is (2.16 + 0.071(70))/0.119 = 59.9 degrees.
#
# Thermal equilibrium at 80W is (2.16 + 5.68)/0.119 = 66.7 degrees.
#
# An over-temperature condition might arise at 70 degrees, i.e. when the
# inverter is throwing off (70(0.119) - 2.16)/(0.071) = 86.9W.


plot(lmDGTL)

gw.heat.hourlH <-
  mutate(gw.heat.hour, LaggedDGPower.0 = lag(mean.DGPower.0)) |>
  slice(-c(1:2)) # a hack, would be better to filter on NA in this field
lmDGTLH <- lm(dT.dt ~ mean.T + mean.DGPower.0 + LaggedDGPower.0,
             gw.heat.hourl0)
summary(lmDGTLH)
plot(lmDGTLH)

# Adding mean.H.Total to the regression is a very slight improvement, maybe just
# collinearity at work! Note that the coefficient on mean.H.total has a very
# small value, just 0.4 degrees of temp rise for every 10k hours.  So we'll
# investigate the simpler model first.
lmDGTHL <- lm(dT.dt ~ mean.T + mean.DGPower + mean.H.Total +
                    lag(mean.DGPower),
                  gw.heat.hour)
summary(lmDGTHL)
# Residuals:
# Min      1Q  Median      3Q     Max
#  -9.3112 -1.1183 -0.0177  1.1038  6.8344
#
# Coefficients:
#                          Estimate   Std. Error  t value  Pr(>|t|)
#  (Intercept)              2.153e+00  7.778e-02   27.68   <2e-16 ***
#  mean.T                  -1.405e-01  4.295e-03  -32.72   <2e-16 ***
#  mean.DGPower       2.970e-07  1.596e-09  186.16   <2e-16 ***
#  mean.H.Total             4.089e-05  2.978e-06   13.73   <2e-16 ***
#  lag(mean.DGPower) -2.175e-07  2.261e-09  -96.20   <2e-16 ***
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
    Residuals = lmDGTL$residuals,
    QYear = factor(quarter(ymd(Date), type = "year.quarter"))
    )
gw.heat.hourl0 <- gw.heat.hourl0 |>
  mutate(
    Residuals = lmDGTL0$residuals,
    QYear = factor(quarter(ymd(Date), type = "year.quarter"))
  )

gw.heat.hourl |> ggplot(aes(Hour, Residuals)) +
  geom_boxplot(aes(group=cut_width(Hour, 1)))
gw.heat.hourl0 |> ggplot(aes(Hour, Residuals)) +
  geom_boxplot(aes(group=cut_width(Hour, 1)))

gw.heat.hourl |> ggplot(aes(Month, Residuals)) +
  geom_boxplot(aes(group=cut_width(Month, 1)))
gw.heat.hourl |> ggplot(aes(QYear, Residuals)) +
  geom_boxplot(aes(group=cut_width(QYear, 1)))
gw.heat.hourl0 |> ggplot(aes(QYear, Residuals)) +
  geom_boxplot(aes(group=cut_width(QYear, 1)))

gw.heat.hourl |> ggplot(aes(mean.T, Residuals)) +
  geom_boxplot(aes(group=cut_width(mean.T, 5)))
gw.heat.hourl0 |> ggplot(aes(mean.T, Residuals)) +
  geom_boxplot(aes(group=cut_width(mean.T, 5)))
gw.heat.hourl |> ggplot(aes(Hour-first.qhour.day, Residuals)) +
  geom_boxplot(aes(group=cut_width(Hour-first.qhour.day, 1)))
gw.heat.hourl0 |> ggplot(aes(Hour-first.qhour.day, Residuals)) +
  geom_boxplot(aes(group=cut_width(Hour-first.qhour.day, 1)))

# Mean residuals are +ve during the first 6h of daily generation, -ve during
# the next 5h, then +ve for the last few hours.  Maybe 20% to 40% of the
# unexplained variance in our .0 model?
gw.heat.hourl |> ggplot(aes(mean.V.MPPT, Residuals)) +
  geom_boxplot(aes(group=cut_width(mean.V.MPPT, 10)))
gw.heat.hourl0 |> ggplot(aes(mean.V.MPPT, Residuals)) +
  geom_boxplot(aes(group=cut_width(mean.V.MPPT, 10)))
# a strong effect here, with -ve residual at voltage below 150V and +ve
# above 190V.  This will be a limitation of the DG model, as it isn't
# sensitive to the nonlinearity of the panels.
lmDGTL0V <- lm(dT.dt ~ mean.T + mean.DGPower.0 + LaggedDGPower.0 +
                mean.V.MPPT,
              gw.heat.hourl0)
summary(lmDGTL0V)
# R-sq = 0.8281, not much improvement



gw.heat.hourl |> ggplot(aes(mean.I.MPPT, Residuals)) +
  geom_boxplot(aes(group=cut_width(mean.I.MPPT, 1)))
gw.heat.hourl0 |> ggplot(aes(mean.I.MPPT, Residuals)) +
  geom_boxplot(aes(group=cut_width(mean.I.MPPT, 1)))

gw.heat.hourl |> ggplot(aes(mean.I.MPPT, Residuals)) +
  geom_boxplot(aes(group=cut_number(mean.I.MPPT, 7)))
gw.heat.hourl0 |> ggplot(aes(mean.I.MPPT, Residuals)) +
  geom_boxplot(aes(group=cut_number(mean.I.MPPT, 7)))
# Our modelling is inaccurate when I.MPPT < 0.5, the inverter apparently throws
# off more heat than expected.  There are many such datapoints, perhaps 30%, so
# they'll be significantly affecting the regression.  Perhaps: fit to the
# higher-current data?  Note: we may be underpredicting slightly at high
# currents (above 6A) but a 0 residual is within the semi-interquartile range.
gw.heat.hourl |> ggplot(aes(mean.I.MPPT*mean.I.MPPT, Residuals)) +
  geom_boxplot(aes(group=cut_width(mean.I.MPPT*mean.I.MPPT, 5)))
gw.heat.hourl0 |> ggplot(aes(mean.I.MPPT*mean.I.MPPT, Residuals)) +
  geom_boxplot(aes(group=cut_width(mean.I.MPPT*mean.I.MPPT, 5)))
# Residual is slightly -ve for highest current flows (above 9A), and also for
# lowest current flows(below 4A), so perhaps we'd do better with a slightly
# lower value for series resistance and a slightly higher value for shunt
# resistance in our DG model of the Goodwe 3000-NS?

gw.heat.hourl |> ggplot(aes(sd.V.MPPT, Residuals)) +
  geom_boxplot(aes(group=cut_number(sd.V.MPPT, 10)))
gw.heat.hourl0 |> ggplot(aes(sd.V.MPPT, Residuals)) +
  geom_boxplot(aes(group=cut_number(sd.V.MPPT, 10)))

# High sd on V.MPPT doesn't greatly affect the residuals. We're underpredicting
# slightly when sd is the lowest decile.


plot(lmDGTL) # high residuals at fitted values below -5
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
# The first significant generation of the day.  DGEnergy was 71Wh, genHour
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


# regressing on an accurate 1-hour lag of DGPower gives marginally better
# results for Rsq but not for residual std error, and we're "throwing away"
# about 10% of the data.  I'll stick with the simpler model.
gw.heat.hourl1 <-
  filter(gw.heat.hour,
         !is.na(lag1H.DG.Power + mean.DGPower + mean.T + dT.dt))
lmDGTL1 <- lm(dT.dt ~ mean.T + mean.DGPower + lag1H.DG.Power,
                gw.heat.hourl1)
summary(lmDGTL1)


# Residuals:
# Min      1Q  Median      3Q     Max
# -9.8268 -1.1180 -0.0017  1.1361  7.0414
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#  (Intercept)         2.795628   0.088745   31.50   <2e-16 ***
#  mean.T             -0.140812   0.004305  -32.71   <2e-16 ***
#  mean.DGPower  0.293829   0.001649  178.15   <2e-16 ***
#  lag1H.DG.Power   -0.220797   0.002276  -97.01   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 1.715 on 7783 degrees of freedom
# Multiple R-squared:  0.8114,	Adjusted R-squared:  0.8113
# F-statistic: 1.116e+04 on 3 and 7783 DF,  p-value: < 2.2e-16

# try regressing on qhour data
gw.heat.qhourl <-
  mutate(gw.heat.qhour, LaggedDGPower = lag(mean.DGPower)) |>
  slice(-c(1:2)) # a hack, would be better to filter on NA in this field

lmDGQTL <- lm(dT.dt ~ mean.T + mean.DGPower + LaggedDGPower,
               gw.heat.qhourl)
summary(lmDGQTL)
# Rsq is 0.60, std error 3.2.  Much less predictive.
plot(lmDGQTL)

gw.heat.qhourl4 <-
  mutate(gw.heat.qhour, LaggedDGPower = lag(mean.DGPower),
         LaggedDGPower2 = lag(mean.DGPower, 2),
         LaggedDGPower3 = lag(mean.DGPower, 3),
         LaggedDGPower4 = lag(mean.DGPower, 4)) |>
  slice(-c(1:5)) # a hack, would be better to filter on NA in this field

lmDGQTL4 <- lm(dT.dt ~ mean.T + mean.DGPower + LaggedDGPower + LaggedDGPower2
               + LaggedDGPower3 + LaggedDGPower4,
              gw.heat.qhourl4)
summary(lmDGQTL4)
# Rsq is 0.71, std error 2.77.  Less predictive, even with four lags.
plot(lmDGQTL)

gw.heat.qhourl4s <-
  mutate(gw.heat.qhour,
         LaggedDGPower = lag(mean.DGPower) + lag(mean.DGPower, 2) +
           lag(mean.DGPower, 3) + lag(mean.DGPower, 4)) |>
  slice(-c(1:5)) # a hack, would be better to filter on NA in this field

lmDGQTL4s <- lm(dT.dt ~ mean.T + mean.DGPower + LaggedDGPower,
               gw.heat.qhourl4s)
summary(lmDGQTL4s)
# Rsq is 0.70, std error 2.78.
plot(lmDGQTL4s)

gw.heat.qhourl4s |>
  ggplot(aes(x=mean.V.MPPT, y=mean.DGPower)) +
  geom_boxplot(aes(group=cut_number(mean.V.MPPT, 10)))
gw.heat.qhourl4s |>
  ggplot(aes(x=mean.V.MPPT, y=dT.dt)) +
  geom_boxplot(aes(group=cut_number(mean.V.MPPT, 10)))
gw.heat.qhourl4s |>
  ggplot(aes(x=mean.V.MPPT, y=sd.V.MPPT)) +
  geom_boxplot(aes(group=cut_number(mean.V.MPPT, 10)))
gw.heat.qhourl4s |>
  ggplot(aes(x=sd.V.MPPT, y=DGEff)) +
  geom_boxplot(aes(group=cut_number(sd.V.MPPT, 10)))


gw.heat.qhourl0 <-
  mutate(gw.heat.qhour, LaggedDGPower.0 = lag(mean.DGPower.0)) |>
  slice(-c(1:2)) # a hack, would be better to filter on NA in this field

lmDGQTL0 <- lm(dT.dt ~ mean.T + mean.DGPower.0 + LaggedDGPower.0,
              gw.heat.qhourl0)
summary(lmDGQTL0)
# Rsq is 0.61, std error 3.2.  Much less predictive.
plot(lmDGQTL0)


# old lms
gw.heat.hour.after9 <- filter(gw.heat.hourm1, Hour>9)
lmDGTHa9 <- lm(dT.dt ~ mean.T + mean.DGPower + mean.H.Total,
                  gw.heat.hour.after9)
summary(lmDGTHa9)
gw.heat.hour.after9 <- bind_cols(gw.heat.hour.after9, lmDGTHa9$residuals) |>
  rename(ResidualsA9 = ...33)
gw.heat.hour.after9 |> ggplot(aes(Hour, ResidualsA9)) +
  geom_boxplot(aes(group=cut_width(Hour, 1)))

gw.heat.hour.a9b5 <- filter(gw.heat.hourm1, Hour>9 & Hour < 17)
lmDGTHa9b5 <- lm(dT.dt ~ mean.T + mean.DGPower + mean.H.Total,
                 gw.heat.hour.a9b5)
summary(lmDGTHa9b5)
gw.heat.hour.a9b5 <- bind_cols(gw.heat.hour.a9b5, lmDGTHa9b5$residuals) |>
  rename(ResidualsA9b5 = ...33)
gw.heat.hour.a9b5 |> ggplot(aes(Hour, ResidualsA9b5)) +
  geom_boxplot(aes(group=cut_width(Hour, 1)))
plot(lmDGTHa9b5)

# Rsq 0.496
lmDGQT <- lm(dT.dt ~ mean.T + mean.DGPower,
               gw.heat.qhour)
summary(lmDGQT)
plot(lmDGQT)
# serious nonlinearity in QQ

# Rsq 0.2124
lmDGAT <- lm(dT.dt ~ Temperature. + DGPower,
               gw.heat)
summary(lmDGAT)
plot(lmDGAT)
# v. serious nonlinearity in QQ.  Maybe need a lagged DGPower?

# Rsq 0.2016, lagging doesn't help
# The temperature sensor must be very close to the SCR!
lmDGLAT <- lm(dT.dt ~ Temperature. + lag(DGPower),
               gw.heat)
summary(lmDGLAT)
