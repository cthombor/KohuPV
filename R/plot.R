#

if (!("tidyverse" %in% (.packages()))) library(tidyverse)
require(ggiraph) # interactive graphs
require(ggiraphExtra) # visualise predictions from multiple regression models

load("data/gw.rda")
load("data/sc.rda")
load("data/sc.all.rda")
load("data/sc.hist.rda")

# DGEnergy stacked with netGenHour

gw.pivot <- gw.heat.hour |>
  pivot_longer(
    cols = c("netGenHour", "DGEnergy"),
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

summary(gw.heat.hour$dT.dt)
maxi <- which.max(gw.heat.hour$dT.dt)
print(paste("Max hour-on-hour temperature rise =",
            round(gw.heat.hour$dT.dt[maxi],1), "degrees/hour, at",
            gw.heat.hour$dateTime[maxi]))
summary(gw.heat$Temperature.)
maxi <- which.max(gw.heat$Temperature.)
print(paste("Max inverter temperature  =",
            round(gw.heat$Temperature.[maxi],1), "degrees Celsius, at",
            gw.heat$dateTime[maxi]))

gw |> ggplot(aes(V.MPPT.1.V.)) +
  geom_histogram(binwidth=5)

gw |> ggplot(aes(V.MPPT.1.V., Power.W.)) +
  geom_boxplot(aes(group=cut_width(V.MPPT.1.V., 5)))

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

# misc plots

ggplot(gw.heat.qhour,
       aes(x=timeOfDay.qhour, y=cumGenDay)) +
  geom_boxplot(aes(group=cut_width(timeOfDay.qhour, 0.25))) +
  facet_wrap(gw.heat.qhour$Month)

ggplot(gw.heat.hour,
       aes(x=Hour, y=DGEnergy)) +
  geom_boxplot(aes(group=cut_width(Hour, 1.0))) +
  facet_wrap(gw.heat.hour$Month)

ggplot(gw.heat.hour,
       aes(x=genHour, y=DGEnergy)) +
  geom_smooth(aes(colour=Year)) +
  facet_wrap(gw.heat.hour$Month)

ggplot(gw.heat.hour,
       aes(DGEnergy, after_stat(density), colour=Year)) +
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
       aes(DGEnergy, after_stat(density), colour=Year)) +
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
       aes(x=Month, y=DGEnergy, colour=Year)) +
  geom_boxplot()

ggplot(gw.heat.hour, aes(x=genHour, y=DGEnergy)) +
  geom_boxplot(aes(group=cut_width(genHour, 0.1)))

ggplot(gw.heat.hour, aes(x=genHour, y=DGEnergy)) +
  geom_boxplot(aes(group=cut_number(genHour, 6)))

ggplot(gw.heat.hour, aes(x=genHour, y=DGEff)) +
  geom_boxplot(aes(group=cut_number(genHour, 6)))
