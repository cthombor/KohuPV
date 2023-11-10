#

if (!("tidyverse" %in% (.packages()))) library(tidyverse)
require(ggiraph) # interactive graphs
require(ggiraphExtra) # visualise predictions from multiple regression models

load("data/gw.heat.rda")
load("data/gw.heat.day.rda")
load("data/gw.heat.hour.rda")
load("data/gw.heat.qhour.rda")

gw.heat |>
  filter(Working.Mode=="Normal") |>
  ggplot(aes(x=V.MPPT.1.V., y=I.MPPT.1.A.)) +
  geom_boxplot(aes(group=cut_width(V.MPPT.1.V., 10)))

gw.heat |>
  filter(Working.Mode=="Normal") |>
  ggplot(aes(x=V.MPPT.1.V., y=I.MPPT.1.A.)) +
  geom_boxplot(aes(group=cut_number(V.MPPT.1.V., 10)))

gw.heat.qhour |>
  ggplot(aes(x=mean.V.MPPT, y=mean.I.MPPT)) +
  geom_boxplot(aes(group=cut_number(mean.V.MPPT, 10)))

summary(gw.heat.qhour$mean.V.MPPT)

gw.heat.qhour |>
  filter(mean.V.MPPT > 180) |>
  ggplot(aes(x=mean.V.MPPT, y=mean.I.MPPT)) +
  geom_boxplot(aes(group=cut_number(mean.V.MPPT, 10)))

gw.heat.hour |>
  ggplot(aes(x=mean.V.MPPT, y=mean.I.MPPT)) +
  geom_boxplot(aes(group=cut_number(mean.V.MPPT, 10)))

gw.heat.hour |>
  filter(mean.V.MPPT > 180) |>
  ggplot(aes(x=mean.V.MPPT, y=mean.I.MPPT)) +
  geom_boxplot(aes(group=cut_number(mean.V.MPPT, 10)))

gw.heat.hour.highv <- gw.heat.hour |>
  filter(mean.V.MPPT > 180)

hist(gw.heat.hour.highv$Hour)

gw.heat.hour |>
  filter(mean.I.MPPT > .1) |>
  filter(mean.V.MPPT < 180) |>
  ggplot(aes(x=mean.V.MPPT, y=mean.I.MPPT)) +
  geom_boxplot(aes(group=cut_number(mean.V.MPPT, 10)))

gw.heat.hour.lowv <- gw.heat.hour |>
  filter(mean.V.MPPT < 130)
hist(gw.heat.hour.lowv$Hour)

gw.heat.hour.7am <- gw.heat.hour |>
  filter(Hour==7)
hist(gw.heat.hour.7am$mean.V.MPPT)
gw.heat.hour.1pm <- gw.heat.hour |>
  filter(Hour==13)
hist(gw.heat.hour.1pm$mean.V.MPPT)
gw.heat.hour.4pm <- gw.heat.hour |>
  filter(Hour==16)
hist(gw.heat.hour.4pm$mean.V.MPPT)
gw.heat.hour.5pm <- gw.heat.hour |>
  filter(Hour==17)
hist(gw.heat.hour.5pm$mean.V.MPPT)
gw.heat.hour.5pm |>
 ggplot(aes(x=mean.V.MPPT)) +
 geom_histogram(binwidth = 10, color = "black", fill = "white")



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
# almost exactly the same fit.

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
# improves its prediction slightly.  However this is a highly nonlinear model
# with several collinear factors, so I'd want to run a reg
temp_modele <- lm(dT.dt ~ genHour + mean.T
                  + mean.H.Total*mean.I.MPPT*genHour,
                  gw.heat.hour)
summary(temp_modele)

# I ca
temp_modelf <- lm(dT.dt ~ mean.T +
                  + CheveletEnergy + genHour,
                  gw.heat.hour)
summary(temp_modelf)



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

ggplot(gw.heat.qhour,
       aes(x=timeOfDay.qhour, y=cumGenDay)) +
  geom_boxplot(aes(group=cut_width(timeOfDay.qhour, 0.25))) +
  facet_wrap(gw.heat.qhour$Month)

ggplot(gw.heat.hour,
       aes(x=Hour, y=CheveletEnergy)) +
  geom_boxplot(aes(group=cut_width(Hour, 1.0))) +
  facet_wrap(gw.heat.hour$Month)

ggplot(gw.heat.hour,
       aes(x=genHour, y=CheveletEnergy)) +
  geom_smooth(aes(colour=Year)) +
  facet_wrap(gw.heat.hour$Month)

ggplot(gw.heat.hour,
       aes(CheveletEnergy, after_stat(density), colour=Year)) +
  geom_freqpoly(binwidth = 0.01) +
  facet_wrap(gw.heat.hour$Month)

ggplot(gw.heat.hour,
       aes(genHour, after_stat(density), colour=Year)) +
  geom_freqpoly(binwidth = 0.2) +
  facet_wrap(gw.heat.hour$Month)

ggplot(gw.heat.day,
       aes(netGen, after_stat(density), colour=Year)) +
  geom_freqpoly(binwidth = 1) +
  facet_wrap(gw.heat.day$Month)

ggplot(gw.heat.hour,
       aes(CheveletEnergy, after_stat(density), colour=Year)) +
  geom_freqpoly(binwidth = 0.05) +
  facet_wrap(gw.heat.hour$Month)

ggplot(gw.heat.hour,
       aes(x=Hour, y=genHour)) +
  geom_boxplot(aes(group=cut_width(Hour, 1.0))) +
  facet_wrap(gw.heat.hour$Month)

ggplot(gw.heat.day,
       aes(x=Month, y=genDay, colour=Year)) +
  geom_boxplot()

ggplot(gw.heat.hour,
       aes(x=Month, y=CheveletEnergy, colour=Year)) +
  geom_boxplot()

ggplot(gw.heat.hour, aes(x=genHour, y=CheveletEnergy)) +
  geom_boxplot(aes(group=cut_width(genHour, 0.1)))

ggplot(gw.heat.hour, aes(x=genHour, y=CheveletEnergy)) +
  geom_boxplot(aes(group=cut_number(genHour, 8)))

ggplot(gw.heat.hour, aes(x=genHour, y=mean.CheveletEff)) +
  geom_boxplot(aes(group=cut_number(genHour, 8)))

gw |> ggplot(aes(V.MPPT.1.V.)) +
  geom_histogram(binwidth=5)

gw |> ggplot(aes(V.MPPT.1.V., Power.W.)) +
  geom_boxplot(aes(group=cut_width(V.MPPT.1.V., 5)))

# defective: errors thrown from fill
gw |> ggplot(aes(x=V.MPPT.1.V., y=I.MPPT.1.A., fill=Year)) +
  geom_boxplot(aes(group=cut_width(V.MPPT.1.V., 5))) +
  facet_wrap(gw$Month)

summary(gw$genDay)
summary(gw$genHour)
summary(gw$genHalfHour)
summary(gw$genQuarterHour)

gwqh <- distinct(gw, pick(Date,Hour,QHour), .keep_all=TRUE)
gwhh <- distinct(gwqh, pick(Date,Hour,HHour), .keep_all=TRUE)
gwh <- distinct(gwhh, pick(Date,Hour), .keep_all=TRUE)
gwd <- distinct(gwh, Date, .keep_all=TRUE)

gwd |>
  ggplot(aes(Month,genDay)) +
  geom_boxplot(aes(group=Month))

gwh |>
  ggplot(aes(x=Hour, y=genHour)) +
  geom_boxplot(aes(group=cut_width(Hour,1))) +
  facet_wrap(gwh$Month)

gwhh |>
  ggplot(aes(x=Hour+HHour/2, y=genHalfHour)) +
  geom_boxplot(aes(group=cut_width(Hour+HHour/2, 0.5))) +
  facet_wrap(gwhh$Month)

gwqh |>
  ggplot(aes(x=Hour+QHour/4, y=genQuarterHour)) +
  geom_boxplot(aes(group=cut_width(Hour+QHour/4, 0.25))) +
  facet_wrap(gwqh$Month)

sc <- mutate(sc, ReportedT.NZ = as.factor(ReportedT.NZ))

ggplot(sc.all, aes(PeriodEnd.NZ,PvEstimate,colour=ReportedT.NZ)) + geom_point()

ggplot(sc, aes(PeriodEnd.NZ,PvEstimate)) + geom_bar(stat="identity")

ggplot(sc.all, aes(PeriodEnd.NZ, PvEstimate)) +
  geom_boxplot(aes(group = cut_width(
    date(PeriodEnd.NZ) +
      hour(PeriodEnd.NZ) +
      trunc(minute(PeriodEnd.NZ) / 30) / 2, 0.5
  )))

ggplot(sc.all, aes(PeriodEnd.NZ,PvEstimate)) +
  geom_blank() +
  stat_summary(fun="mean",geom="bar")

ggplot(sc.hist, aes(hour(PeriodEnd.NZ), ghi)) +
  geom_boxplot(aes(group = cut_width(hour(PeriodEnd.NZ),1)))

sc.hist |>
  ggplot(aes(hour(PeriodEnd.NZ), ghi)) +
  geom_boxplot(aes(group = cut_width(hour(PeriodEnd.NZ), 1))) +
  facet_wrap(sc.hist$Month)

summary(gw$V.MPPT.1.V.)
summary(gw$I.MPPT.1.A.)
summary(gw$Power.W.)
gw$Time[which.max(gw$Power.W.)]
