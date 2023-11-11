library(tidyverse)
library(readxl)

# 10y data from solcast pro, massage for import into pvsyst amd analysis
sc.hist <-
  read_csv("data-raw/pvsyst/scd.csv", col_types="iiiTc")
sc.hist <- sc.hist |>
  mutate(PeriodEnd.NZ = with_tz(period_end, tz = "NZ")) |>
  mutate(Period = dhours(1)) |>
  select(-period) |>
  rename(PeriodEnd.UTC = period_end) |>
  mutate(
    Year = factor(year(PeriodEnd.NZ)),
    Month = factor(months(PeriodEnd.NZ, abbreviate = TRUE), levels = month.abb),
    Day = factor(day(PeriodEnd.NZ)),
    Hour = hour(PeriodEnd.NZ)
  )
usethis::use_data(sc.hist, overwrite = TRUE)

sc.hist0 <- sc.hist |>
  mutate(Year=year(PeriodEnd.NZ)) |>
  mutate(Month=month(PeriodEnd.NZ)) |>
  mutate(Month.abbr=months(PeriodEnd.NZ)) |>
  mutate(Day=day(PeriodEnd.NZ)) |>
  mutate(Hour=hour(PeriodEnd.NZ)) |>
  relocate(Year:Hour) |>
  select(Year:ghi, PeriodEnd.UTC, PeriodEnd.NZ)
write_csv(sc.hist0,"data-raw/pvsyst/sc.hist0.csv") # maybe useful someday?

# recent 1y data from solcast pro, massage for import into pvsyst amd analysis
sc.hist.recent0 <- sc.hist0 |>
  filter(PeriodEnd.NZ <= force_tz(ymd("2023-09-01"), tz="NZ")) |>
  filter(PeriodEnd.NZ > force_tz(ymd("2022-09-01"), tz="NZ"))
write_csv(sc.hist.recent0,"data-raw/pvsyst/sc.hist.recent0.csv")
sc.hist.recent <- sc.hist |>
  filter(PeriodEnd.NZ <= force_tz(ymd("2023-09-01"), tz="NZ")) |>
  filter(PeriodEnd.NZ > force_tz(ymd("2022-09-01"), tz="NZ"))
usethis::use_data(sc.hist.recent, overwrite=TRUE)

#5d data from solcast free
sc.all <- NULL
for (fn in dir("data-raw/solcast")) {
  sc1 <- read_csv(
    paste0("data-raw/solcast/", fn),
    name_repair = "universal",
    col_types = "dTc")
  sc1 <- bind_cols(sc1, tibble(ReportedT.NZ=ymd_hm(substr(fn,1,14), tz="NZ")))
  if (is.null(dim(sc.all))) {
    sc.all <- sc1
  } else {
    sc.all <- bind_rows(sc.all, sc1)
  }
}

sc.all <- sc.all |>
  arrange(PeriodEnd, ReportedT.NZ) |>
  rename(PeriodEnd.UTC = PeriodEnd) |>
  mutate(PeriodEnd.NZ = with_tz(PeriodEnd.UTC, tz="NZ"))

sc <- sc.all |>
  group_by(PeriodEnd.NZ) |>
  mutate(PvEstimate = last(PvEstimate)) |>
  distinct(PeriodEnd.NZ, .keep_all = TRUE) |>
  select(PeriodEnd.NZ, PvEstimate, ReportedT.NZ)

rm(sc1,fn)

usethis::use_data(sc.all, overwrite=TRUE)
usethis::use_data(sc, overwrite=TRUE)

