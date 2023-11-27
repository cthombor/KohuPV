if (!("tidyverse" %in% (.packages()))) library(tidyverse)
require(patchwork)

load("data/gw.heat.rda")
load("data/gw.heat.day.rda")
load("data/gw.heat.hour.rda")
load("data/gw.heat.qhour.rda")
load("data/recent.trinasim.hourly.rda")

gw.heat <- gw.heat |>
  mutate(as.factor(Working.Mode))
scale_color_WM <- scale_color_manual(values = c(
  "Wait" = "orange",
  "Normal" = "green",
  "Fault" = "red"
))

gw.heat.iso.fail.4.33pm <- gw.heat |>
  filter(Date=="2023-11-20")
gw.heat.iso.fail.4.33pm |>
  ggplot(aes(x=Hour, y=V.MPPT.1.V.)) +
  geom_boxplot(aes(group=cut_width(Hour,1)))
gw.heat.iso.fail.4.33pm |>
  mutate(time=hms::as_hms(dateTime)/dhours(1)) |>
  ggplot(aes(x=time, y=V.MPPT.1.V.)) +
  geom_point(aes(color=Working.Mode)) + scale_color_WM +
  labs(title="V.MPPT by time of day, on 2023-11-20. Iso fail at 4:33pm")

gw.heat.iso.fail.7.06am <- gw.heat |>
  filter(Date=="2023-10-07")
gw.heat.iso.fail.7.06am |>
  ggplot(aes(x=Hour, y=V.MPPT.1.V.)) +
  geom_boxplot(aes(group=cut_width(Hour,1)))
gw.heat.iso.fail.7.06am |>
  mutate(time=hms::as_hms(dateTime)/dhours(1)) |>
  ggplot(aes(x=time, y=V.MPPT.1.V.)) +
  geom_point(aes(color=Working.Mode)) + scale_color_WM +
  labs(title="V.MPPT by time of day, on 2023-10-07. Iso fail at 7:06am")

gw.heat.fac.fail.2.18pm <- gw.heat |>
  filter(Date=="2023-10-02")
gw.heat.fac.fail.2.18pm |>
  mutate(time=hms::as_hms(dateTime)/dhours(1)) |>
  ggplot(aes(x=time, y=V.MPPT.1.V.)) +
  geom_point(aes(color=Working.Mode)) + scale_color_WM +
  labs(title="V.MPPT by time of day, on 2023-10-02. Iso fail at 2:18pm")

gw.heat.3.Oct <- gw.heat |>
  filter(Date=="2023-10-03")
gw.heat.3.Oct |>
  mutate(time=hms::as_hms(dateTime)/dhours(1)) |>
  ggplot(aes(x=time, y=V.MPPT.1.V.)) +
  geom_point(aes(color=Working.Mode)) + scale_color_WM +
  labs(title="V.MPPT by time of day, on 2023-10-03")

gw.heat.iso.fail.6.49pm <- gw.heat |>
  filter(Date=="2023-09-15")
gw.heat.iso.fail.6.49pm |>
  mutate(time=hms::as_hms(dateTime)/dhours(1)) |>
  ggplot(aes(x=time, y=V.MPPT.1.V.)) +
  geom_point(aes(color=Working.Mode)) + scale_color_WM +
  labs(title="V.MPPT by time of day, on 2023-09-15. Iso fail at 6:49pm")

gw.heat.fac.fail.9.06am <- gw.heat |>
  filter(Date=="2023-08-30") |>
  inner_join(recent.trinasim.hourly, by=c("Year", "Month", "Day", "Hour"))
p1 <- gw.heat.fac.fail.9.06am |>
  ggplot(aes(x=Hour, y=V.MPPT.1.V.)) +
  geom_boxplot(aes(group=cut_width(Hour,1)))
p2 <- gw.heat.fac.fail.9.06am |>
  mutate(time=hms::as_hms(dateTime)/dhours(1)) |>
  ggplot(aes(x=time, y=V.MPPT.1.V.)) +
  geom_point(aes(color=Working.Mode)) + scale_color_WM +
  labs(title="V.MPPT by time of day, on 2023-08-30. Fac fail at 9:06am")
p2


gw.heat.1.Aug <- gw.heat |>
  filter(Date=="2023-08-01") |>
  inner_join(recent.trinasim.hourly, by=c("Year", "Month", "Day", "Hour")) |>
  pivot_longer(
    cols = c("genHour", "E_Avail"),
    names_to = "energy",
    values_to = "kWh"
  )
p1 <- gw.heat.1.Aug |>
  mutate(time=hms::as_hms(dateTime)/dhours(1)) |>
  ggplot(aes(x=time, y=V.MPPT.1.V.)) +
  geom_point(aes(color=Working.Mode), size=1) + scale_color_WM
p2 <- gw.heat.1.Aug |>
  mutate(time=hms::as_hms(dateTime)/dhours(1)) |>
  ggplot(aes(x = time, y = kWh, color = energy, group = energy)) + geom_line()
p1 + labs(title = "V.MPPT, E_Avail, and genHour on 2023-08-01") + p2 +
  plot_layout(ncol = 1)

gw.heat.1.Feb <- gw.heat |>
  filter(Date=="2023-02-01") |>
  inner_join(recent.trinasim.hourly, by=c("Year", "Month", "Day", "Hour")) |>
  pivot_longer(
    cols = c("genHour", "E_Avail"),
    names_to = "energy",
    values_to = "kWh"
  )
p1 <- gw.heat.1.Feb |>
  mutate(time=hms::as_hms(dateTime)/dhours(1)) |>
  ggplot(aes(x=time, y=V.MPPT.1.V.)) +
  geom_point(aes(color=Working.Mode), size=1) + scale_color_WM
p2 <- gw.heat.1.Feb |>
  mutate(time=hms::as_hms(dateTime)/dhours(1)) |>
  ggplot(aes(x = time, y = kWh, color = energy, group = energy)) + geom_line()
p1 + labs(title = "V.MPPT, E_Avail, and genHour on 2023-02-01") + p2 +
  plot_layout(ncol = 1)

p3 <- gw.heat.1.Feb |>
  filter(Hour==9) |>
  mutate(time=hms::as_hms(dateTime)/dhours(1)) |>
  ggplot(aes(x=time, y=V.MPPT.1.V.)) +
  geom_point(aes(color=Working.Mode), size=1) + scale_color_WM
p4 <- gw.heat.1.Feb |>
  filter(Hour==9) |>
  mutate(time=hms::as_hms(dateTime)/dhours(1)) |>
  ggplot(aes(x = time, y = Power.W.)) + geom_line()
p3 + labs(title = "V.MPPT and inverter Power, 9am to 10am on 2023-02-01") +
  p4 + plot_layout(ncol = 1)

gw.heat.1.Nov.22 <- gw.heat |>
  filter(Date=="2022-11-01") |>
  inner_join(recent.trinasim.hourly, by=c("Year", "Month", "Day", "Hour")) |>
  pivot_longer(
    cols = c("genHour", "E_Avail"),
    names_to = "energy",
    values_to = "kWh"
  )
p1 <- gw.heat.1.Nov.22 |>
  mutate(time=hms::as_hms(dateTime)/dhours(1)) |>
  ggplot(aes(x=time, y=V.MPPT.1.V.)) +
  geom_point(aes(color=Working.Mode), size=1) + scale_color_WM
p2 <- gw.heat.1.Nov.22 |>
  mutate(time=hms::as_hms(dateTime)/dhours(1)) |>
  ggplot(aes(x = time, y = kWh, color = energy, group = energy)) + geom_line()
p1 + labs(title = "V.MPPT, E_Avail, and genHour on 2022-11-01") + p2 +
  plot_layout(ncol = 1)






# errtmax <- max(gw.heat.hour.recent$errTrinaPred)
errtmaxw <- which(round(gw.heat.hour.recent$errTrinaPred,1) == 0.6)[1]
errtmaxd <- as.character.Date(gw.heat.hour.recent$Date[errtmaxw])
print(paste0("Max prediction error on Trina model (recent) = ", 0.6,
             "kWh at ", gw.heat.hour.recent$dateTime[errtmaxw]))
gw.heat.errTrinaPred.600Wh.at.2pm <- gw.heat |>
  filter(Date==errtmaxd) |>
  inner_join(recent.trinasim.hourly,
             by=c("Year", "Month", "Day", "Hour"))
gw.heat.errTrinaPred.600Wh.at.2pm |>
  ggplot(aes(x=Hour, y=V.MPPT.1.V.)) +
  geom_boxplot(aes(group=cut_width(Hour,1)))


gw.heat.hour.recent |> ggplot(aes(errTrinaPred)) +
  geom_histogram()

gw.heat.hour.recent |>
  #  filter(errTrinaPred > 0.3) |>
  ggplot(aes(x=max.V.MPPT, y=mean.V.MPPT)) +
  geom_boxplot(aes(group=cut_number(max.V.MPPT, 5)))

gw.heat.hour.recent |>
  ggplot(aes(x=Hour, y=max.V.MPPT)) +
  geom_boxplot(aes(group=cut_number(Hour, 8)))

gw.heat.hour.recent |>
  filter(errTrinaPred > 0.6) |>
  ggplot(aes(x=Hour, y=max.V.MPPT)) +
  geom_boxplot(aes(group=cut_number(Hour, 8)))

gw.heat.hour.recent |>
  filter(errTrinaPred > 0.4) |>
  ggplot(aes(x=Hour, y=max.V.MPPT)) +
  geom_boxplot(aes(group=cut_number(Hour, 8)))

gw.heat.hour.recent |>
  filter(errTrinaPred > 0.2) |>
  ggplot(aes(x=Hour, y=max.V.MPPT)) +
  geom_boxplot(aes(group=cut_number(Hour, 8)))
