library(tidyverse)
library(readxl)

augsep23.peimarsim.hourly.horizon <-
  read_xlsx("data-raw/pvsyst/peimar augsep 23 geo horiz.xlsx") |>
  mutate(
    PeriodEnd = date + dhours(2), # for consistency with my hourly summaries
    Year = factor(year(PeriodEnd)),
    Month = factor(months(PeriodEnd, abbreviate = TRUE), levels = month.abb),
    Day = factor(day(PeriodEnd)),
    Hour = hour(PeriodEnd)
  )
usethis::use_data(augsep23.peimarsim.hourly.horizon, overwrite=TRUE)

firstyear.peimarsim.hourly.horizon <-
  read_xlsx("data-raw/pvsyst/peimar first year geo horiz.xlsx") |>
  mutate(
    PeriodEnd = date + dhours(2), # for consistency with my hourly summaries
    Year = factor(year(PeriodEnd)),
    Month = factor(months(PeriodEnd, abbreviate = TRUE), levels = month.abb),
    Day = factor(day(PeriodEnd)),
    Hour = hour(PeriodEnd)
  )
usethis::use_data(firstyear.peimarsim.hourly.horizon, overwrite=TRUE)

recent.peimarsim.hourly.horizon <-
  read_xlsx("data-raw/pvsyst/Peimar recent horiz.xlsx") |>
  mutate(
    PeriodEnd = date + dhours(2), # for consistency with my hourly summaries
    Year = factor(year(PeriodEnd)),
    Month = factor(months(PeriodEnd, abbreviate = TRUE), levels = month.abb),
    Day = factor(day(PeriodEnd)),
    Hour = hour(PeriodEnd)
  )
usethis::use_data(recent.peimarsim.hourly.horizon, overwrite=TRUE)

recent.peimarsim.hourly <-
  read_xlsx("data-raw/pvsyst/Peimar300HourlyRecent.xlsx") |>
  mutate(
    PeriodEnd = date + dhours(2), # for consistency with my hourly summaries
    Year = factor(year(PeriodEnd)),
    Month = factor(months(PeriodEnd, abbreviate = TRUE), levels = month.abb),
    Day = factor(day(PeriodEnd)),
    Hour = hour(PeriodEnd)
  )
usethis::use_data(recent.peimarsim.hourly, overwrite=TRUE)

recent.trinasim.hourly <-
  read_xlsx("data-raw/pvsyst/Trina270HourlyRecent.xlsx") |>
  mutate(
    PeriodEnd = date + dhours(2),
    Year = factor(year(PeriodEnd)), # for consistency with my hourly summaries
    Month = factor(months(PeriodEnd, abbreviate = TRUE), levels = month.abb),
    Day = factor(day(PeriodEnd)),
    Hour = hour(PeriodEnd)
  )
usethis::use_data(recent.trinasim.hourly, overwrite=TRUE)

# simulated daily output, from pvsyst with Peimar 300W panels
recent0.daily <-
  read_xlsx("data-raw/pvsyst/daily prod simul recent0.xlsx") |>
  mutate(Year = factor(year(Date)),
         Month = factor(months(Date, abbreviate=TRUE), levels = month.abb),
         Day = factor(day(Date))
  )
usethis::use_data(recent0.daily, overwrite=TRUE)

# simulated daily output, from pvsyst with Trina 270W panels, Goodwe 2000-NS
trina270.daily <-
  read_xlsx("data-raw/pvsyst/trina270.xlsx") |>
  mutate(Year = factor(year(Date)),
         Month = factor(months(Date, abbreviate=TRUE), levels = month.abb),
         Day = factor(day(Date))
  )
usethis::use_data(trina270.daily, overwrite=TRUE)

recent0.daily |>
  ggplot(aes(Month,kWh)) +
  geom_boxplot(aes(group=Month)) +
  labs(caption=
      "Simulated Peimar 300 / Goodwe 3000 NS for Sept 2022 through Aug 2023")
recent0.daily |>
  ggplot(aes(Month,kWh)) +
  geom_blank() +
  stat_summary(fun="mean",geom="bar") +
  labs(caption=
    "Simulated Peimar 300 / Goodwe 3000 NS for Sept 2022 through Aug 2023")

trina270.daily |>
  ggplot(aes(Month,kWh)) +
  geom_boxplot(aes(group=Month)) +
  labs(caption=
      "Simulated Trina 270 / Goodwe 2000 NS for Sept 2022 through Aug 2023")
trina270.daily |>
  ggplot(aes(Month,kWh)) +
  geom_blank() +
  stat_summary(fun="mean",geom="bar") +
  labs(caption=
      "Simulated Trina 270 / Goodwe 2000 NS for Sept 2022 through Aug 2023")

