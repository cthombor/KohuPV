library(tidyverse)
library(readxl)

gw <- NULL
dn <- "data-raw/xls-withoutSS/"
for (fn in dir(dn)) {
  pn <- paste0(dn, fn)
  gw1.names <- read_excel(pn, skip=2, n_max=0)
  gw1 <- read_excel(
    pn,
    skip = 2,
    .name_repair = "universal",
    col_types = c(
      "text",
      "text",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric"
    ))
  gw1 <- gw1 |>
    mutate(
      # As at Oct 2023, Goodwe's clock was -10min.  I'll assume this isn't
      # drift, nor an artefact of power-outages, but instead was a careless
      # initial setup.
      timeAdjust = dminutes(10)) |>
    mutate(
      dateTime = ymd_hms(Time, tz="NZ") + timeAdjust,
      Source = pn) |>
    mutate(
      timeStepLag = as.duration(interval(lag(dateTime), dateTime))) |>
    # older logfiles are on 5min intervals
    mutate(
      logInterval = as.duration(summary(timeStepLag)[["1st Qu."]]))
  gw2 <- distinct(gw1, dateTime)
  stopifnot(nrow(gw1) == nrow(gw2))
 #   cat(paste(nrow(gw1)-nrow(gw2), "duplicate timestamps in", pn)))

  if (is.null(dim(gw))) {
    gw <- gw1
    gw.names <- gw1.names
  } else if (isTRUE(all.equal(gw.names, gw1.names))) {
    gw <- bind_rows(gw, gw1)
  } else {
    stop("Mismatched column names")
  }
}

gw2 <- distinct(gw, dateTime)
if (nrow(gw) > nrow(gw2)) {
  gw <- arrange(gw, dateTime)
  wm <- first(which(gw$dateTime[1:nrow(gw)-1] == gw$dateTime[2:nrow(gw)]))
  stop(
    nrow(gw) - nrow(gw2),
    " duplicate timestamps, first at ",
    wm,
    ": ",
    gw$dateTime[wm]
  )
}

dn <- "data-raw/xls-dmy/"
for (fn in dir(dn)) {
  pn <- paste0(dn, fn)
  gw1 <- read_excel(
    pn,
    skip = 2,
    .name_repair = "universal",
    col_types = c(
      "text",
      "text",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric"
    )
  )
  gw1 <- gw1 |>
    mutate(Time = paste(mdy_hms(Time,tz="NZ"))) |>
    mutate(timeAdjust = dminutes(10)) |>
    mutate(dateTime = ymd_hms(Time, tz="NZ") + timeAdjust) |>
    mutate(Source = pn) |>
    mutate(timeStepLag = as.duration(interval(lag(dateTime), dateTime))) |>
    # older logfiles are on 5min intervals
    mutate(logInterval = as.duration(summary(timeStepLag)[["1st Qu."]]))

  gw2 <- distinct(gw1, dateTime)
  if (nrow(gw1) > nrow(gw2)) {
    cat(paste(pn, nrow(gw1)-nrow(gw2)))
  }
  gw <- bind_rows(gw, gw1)
}

gw <- gw %>%
  mutate(shadowScan = FALSE)

gwss <- NULL
dn <- "data-raw/xls-withSS/"
for (fn in dir(dn)) {
  pn <- paste0(dn, fn)
  gw1 <- read_excel(
    pn,
    skip = 2,
    .name_repair = "universal",
    col_types = c(
      "text",
      "text",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric"
    )
  )
  gw1 <- gw1 |>
    mutate(timeAdjust = dminutes(10)) |>
    mutate(dateTime = ymd_hms(Time, tz="NZ") + timeAdjust) |>
    mutate(Source = pn) |>
    mutate(timeStepLag = as.duration(interval(lag(dateTime), dateTime))) |>
    # older logfiles are on 5min intervals
    mutate(logInterval = as.duration(summary(timeStepLag)[["1st Qu."]]))
  gw2 <- distinct(gw1, dateTime)
  stopifnot(nrow(gw1) == nrow(gw2))

  if (nrow(gw1) > nrow(gw2)) {
    cat(paste(pn, nrow(gw1)-nrow(gw2)))
  }
  if (is.null(dim(gwss))) {
    gwss <- gw1
  } else {
    gwss <- bind_rows(gwss, gw1)
  }
}

gwss <- gwss %>%
  mutate(shadowScan = TRUE)

gw <- bind_rows(gw, gwss) |>
  rename(Temperature. = Temperature...) |>
  arrange(dateTime) |>
  # get a complete set of timeStepLag
  mutate(
    timeStepLag = as.duration(interval(lag(dateTime), dateTime))
  )

gw2 <- distinct(gw, dateTime)
if (nrow(gw) > nrow(gw2)) {
  wm <- first(which(gw$dateTime[1:nrow(gw)-1] == gw$dateTime[2:nrow(gw)]))
  stop(
    nrow(gw) - nrow(gw2),
    " duplicate timestamps, first at ",
    wm,
    ": ",
    gw$dateTime[wm]
  )
}

stopifnot(any(gw$timeStepLag > dseconds(20), na.rm=TRUE))
mts <- which.min(gw$timeStepLag)
cat(paste("Minimal timestep of", gw$timeStepLag[mts], "at",
          gw$dateTime[mts], "-", gw$timeAdjust[mts], "in", gw$Source[mts]))
# There are a couple of ultra-short timesteps in firstweeks.xlsx:
# 6/7/2018 12:11:24 PM
# 6/7/2018 12:14:02 PM  # amended to "6/7/2018 12:13:02 PM"
# 6/7/2018 12:14:06 PM
#
# 4/15/2018 12:48:53 PM
# 4/15/2018 12:49:32 PM # deleted
# 4/15/2018 12:49:43 PM
#
#There are also thousands of timesteps < 30s.  None shorter than 20s,
# aside from the above.

cat("nFaults:", sum(gw$Working.Mode=="Fault"))
cat("nWaits:", sum(gw$Working.Mode=="Wait"))

# The first timeStepLag in an imported xlsx file is assumed to be of
# duration gw$logInterval, as is every timeStepLag which follows a non-Normal
# log entry.
# gw$timeStepLag <-
#  if_else(((lag(gw$Working.Mode) == "Fault") |
#            (lag(gw$Working.Mode) == "Wait")),
#         gw$logInterval, gw$timeStepLag, gw$logInterval)
# we're considering only timesteps in the "Normal" or "Wait" state
# gw$timeStepLag <-
#  if_else((gw$Working.Mode == "Fault"),
#          NA, gw$timeStepLag)

# obsolete code, indicates how timeStepLag had been computed
# gw$timeStepLead <-
#  if_else(((lead(gw$Working.Mode) == "Fault") |
#             (lead(gw$Working.Mode) == "Wait")),
#          dseconds(61), gw$timeStepLead, dseconds(0))

stopifnot(nrow(gw) == nrow(distinct(gw,Time)))

sum(gw$timeStepLag > 2*gw$logInterval, na.rm=TRUE)
sum(gw$timeStepLag > 4*gw$logInterval, na.rm=TRUE)
sum(gw$timeStepLag > 8*gw$logInterval, na.rm=TRUE)
sum(gw$timeStepLag > 16*gw$logInterval, na.rm=TRUE)
sum(gw$timeStepLag > 32*gw$logInterval, na.rm=TRUE)

# code to identify anomalously-long timesteps
mts <- which.max(gw$timeStepLag / gw$logInterval) # max relative to logInterval
cat("Maximal timestep", gw$timeStepLag[mts], "at", gw$Time[mts], "in",
    gw$Source[mts])

# Maximal timestep 7756 at 2023.01.04 15:55:25 in
# data-raw/xls-withoutSS/Historical Data Export-20231001074447rev.xls
# Offline for 2h, apparently: no change in total kWh between two log-entries
# and temp dropped by 2.5 degrees
# 2023.01.04 13:46:09	Normal
# 2023.01.04 15:56:25	Normal
# I have introduced a Fault at 2023.01.04 15:55:25, temp 24.4

# Maximal timestep 5536 at 2018-05-24 12:56:41
# in data-raw/xls-dmy/firstweeks.rev.xlsx
# Temp dropped by 6.4 degrees, no change in total kWh
# 5/24/2018 11:24:25 AM	Normal
# 5/24/2018 12:57:41 PM	Normal
# I have introduced a Fault at 5/24/2018 12:56:40, temp 17.1

# Maximal timestep 5182 at 2023.10.02 15:56:05
# in data-raw/xls-withSS/Historical Data Export-20231003130819.xls
# 2023.10.02 14:29:43	Normal
# 2023.10.02 15:56:05	Wait
# 2023.10.02 15:57:06	Normal
# Temp drops 10.4 degrees (to 26.3) during this long Wait.  This will be an
# interesting datapoint -- inverter perhaps shut down for 25 minutes then wakes
# up. Its standby power may be 0 during most of this period, hence maybe an
# underpredicted cooldown?
# Retained (but flagged in yellow in the xlsx)

# Maximal timestep 3471 at 2018-05-24 14:07:08
# in data-raw/xls-dmy/firstweeks.rev.xlsx
# A 1-hour Wait.  Temp cools from 22.1 to 19.5, which will be difficult to
# model accurately without knowledge of the ambient temperature in the hallway.
# Retained (but flagged in yellow in the xlsx)

# Maximal timestep 2821 at 2023.01.28 09:43:29
# in data-raw/xls-withoutSS/Historical Data Export-20231001074714.xls
# 2023.01.28 08:55:27	Wait, 24	8604
# 2023.01.28 08:56:28	Normal, 24	8604
# 2023.01.28 09:43:29	Wait, 25.6	8604.1
# Temp rose by 1.6 (to 25.6) during the interval from 8:56:28 and 9:43:29, and
# there was some generation.  Earlier that morning, temp was rising slowly in
# Wait states, e.g. 1.0 degree from 7:20 to 7:39 (from 19.7 to 20.7), and from
# 7:39 to 7:59.  Hall temp was maybe 19.7 degrees (this being the first temp
# logged that morning).  Later that morning, multi-minute Waits didn't cause
# much temperature change, with temp staying at 25.3 to 25.5.  So maybe just
# one missing log-entry, or an artefact of the total.kWh field being computed
# somewhat before the other fields in the Normal log-entry?
# Retained (but flagged in yellow in the xlsx)

# Maximal timestep 1154 at 2018-05-23 13:31:59
# in data-raw/xls-dmy/firstweeks.rev.xlsx
# 5/23/2018 1:12:46 PM	Normal
# 5/23/2018 1:31:59 PM	Normal
# Temp dropped 1 degree, kWh stable
# I have introduced a Wait at 5/23/2018 1:30:58 PM, temp 22.7
# this temp sets up a 0.2 degree temp rise in the following Normal

# Maximal timestep 1102 at 2023.06.24 15:45:02
# in data-raw/xls-withoutSS/Historical Data Export-20230928154515flagged.xls
# 2023.06.24 15:26:40	Normal
# 2023.06.24 15:45:02	Normal
# total kWh and temp have risen, so maybe just a 21m gap in logging?
# Retained (but flagged in yellow in the xlsx)

# Maximal timestep 518 at 2023.01.28 13:27:35
# in data-raw/xls-withoutSS/Historical Data Export-20231001074714.xls
# 2023.01.28 13:18:57	Normal	166.4, 38.3	8605.6
# 2023.01.28 13:27:35	Wait	204.5, 35.8	8605.6
# 2023.01.28 13:28:36	Normal	149, 35.5	8605.6
# Retained (but flagged in yellow in the xlsx)

# Maximal timestep 2700 at 2018.09.21 17:49.45 in
# data-raw/xls-withoutSS/Historical Data Export-20231027155446rev.xls
# 2018.09.21 17:05:45	Normal
# 2018.09.21 17:49:45	Normal
# 2018.09.21 17:50:46	Normal
# temp dropped 1 degree. Introduced a wait at 2018.09.21 17:45:15, temp 28.9

# Maximal timestep 365 at 2023.01.18 18:04:40
# in data-raw/xls-withoutSS/Historical Data Export-20231001074618.xls
# 2023.01.18 17:58:35	Normal, 679	49.5
# 2023.01.18 18:04:40	Normal, 620	48.8
# seems rather a large temp drop for 650W over 6 min, but it's in keeping
# with neighbouring temp drops, so retained (but flagged).

# Maximal timestep 333 at 2018-07-13 15:09:43
# in data-raw/xls-dmy/firstweeks.rev.xlsx
# 7/13/2018 3:04:10 PM	Normal, 105	27.6
# 7/13/2018 3:09:43 PM	Normal, 96	27.1
# Retained (but flagged).

# Maximal timestep 296 at 2018-05-23 12:45:39
# in data-raw/xls-dmy/firstweeks.rev.xlsx
# 5/23/2018 12:40:43 PM	Normal, 190	23.0
# 5/23/2018 12:45:39 PM	Wait, 0	23.0
# 5/23/2018 12:46:40 PM	Normal, 300	22.6
# Retained (but flagged).

# Maximal timestep 248 at 2022.12.01 09:25:12
# in data-raw/xls-withoutSS/Historical Data Export-20231001074247.xls
# 2022.12.01 09:21:04	Normal, 319	26.4
# 2022.12.01 09:25:12	Wait, 0	26
# 2022.12.01 09:26:13	Normal, 460	25.8
# Retained (but flagged).

gw <- gw |>
  mutate(Date = factor(date(dateTime)),
         Year = factor(year(dateTime)),
         Month = factor(months(dateTime, abbreviate=TRUE), levels = month.abb),
         Day = factor(day(dateTime)),
         Hour = hour(dateTime)
  )


gw1 <- gw |>
  # The inverter sometimes produce power even when its timesteps are gappy. Some
  # apparently-normal timesteps are more than 100 seconds long. For example,
  # 2023-01-18 at 6pm apparently had a 6-minute step at 620W; inverter temp
  # and Power.W. dropped at about the same rate as over the preceding six
  # (normal-length) steps, so I'll treat it as missing data that's imputed
  # as the same as the following reading.  Another possibility would be to
  # linearly interpolate; but there aren't enough steps longer than 1.1 minutes
  # to validate any such adjustment, and the adjustment is quite small.
  #
  # Two timesteps (Time = 2018-05-23 13:31:59, 2023.06.24 15:45:02) are longer
  # than 18 minutes.  Treating them as 10-minute steps brings the min, max,
  # and mean of genDayiff to within cooee of 0.0.  Possibly the best estimate
  # would be to set dt = timeStepLag/2 whenever the power is low and the
  # timestep is extraordinarily long, on the theory that an instantaneous
  # measure of Power.W. is an overestimate of its mean value when the MPPT
  # is causing the power to swing wildly between 0W and 200W.
  # mutate(dt = ifelse(timeStepLag > dseconds(600),
  #               600, timeStepLag / dseconds(1))) |>
  mutate(dt = timeStepLag / dseconds(1)) |>
  group_by(Date) |>
  mutate(
    genDay = sum(Power.W. * dt / 3600, na.rm=TRUE) / 1000,
    genDay1 = max(Total.Generation.kWh.) - min(Total.Generation.kWh.),
    genDayDiff = genDay - genDay1
  ) |>
  ungroup()

# select(filter(gw1, Total.Generation.kWh.==0), c(Date,Total.Generation.kWh.))

summary(gw1$genDayDiff)
# prior to correcting two spurious 0 entries for Total.Generation.kWh. in
# xls-withoutSS/Historical Data Export-20231027155120.xls :
#
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# -6554.500    -6.600    -4.200    -8.119    -2.400     0.000
#
# After repairing xls-withoutSS/Historical Data Export-20231027155120.xls :
#
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# -0.736669 -0.042930 -0.002413 -0.007085  0.032799  0.712067
#
# After further repairs, as documented above, genDay is strongly validated
# as a higher-precision value than a daily diff on Total.Generation.kWh:
#
#       Min.    1st Qu.     Median       Mean    3rd Qu.       Max.
# -0.7149339 -0.0412086  0.0006164 -0.0005237  0.0344739  0.7357597

gw <- gw1 |>
  mutate(HHour = trunc(minute(dateTime) / 30),
         QHour = trunc(minute(dateTime) / 15)) |>

  group_by(Date, Hour) |>
  mutate(
    genHour = sum(Power.W. * dt / 3600, na.rm=TRUE) / 1000,
    genHourdiff =
      genHour - max(Total.Generation.kWh., na.rm=TRUE) +
      min(Total.Generation.kWh., na.rm=TRUE)
  ) |>

  group_by(Date, Hour, HHour) |>
  mutate(
    genHalfHour = sum(Power.W. * dt / 3600, na.rm=TRUE) / 1000,
    genHalfHourdiff =
      genHalfHour - max(Total.Generation.kWh., na.rm = TRUE) +
      min(Total.Generation.kWh., na.rm = TRUE)
  ) |>

  group_by(Date, Hour, QHour) |>
  mutate(
    genQuarterHour = sum(Power.W. * dt / 3600, na.rm = TRUE) / 1000,
    genQuarterHourdiff =
      genQuarterHour - max(Total.Generation.kWh., na.rm = TRUE) +
      min(Total.Generation.kWh., na.rm = TRUE)
  ) |>

  ungroup()

# our short-interval estimates of energy is slightly biased upwards:
summary(gw$genHourdiff)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#   -0.211233 -0.015140  0.007693  0.010122  0.034072  0.404918

summary(gw$genHalfHourdiff)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#   -0.13   -0.01    0.01    0.02    0.04    0.38  125409

summary(gw$genQuarterHourdiff)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# -0.13   -0.01    0.00    0.01    0.04    0.31   96452

rm(gw1,gw2,gw1.names,gwss,dn,fn,pn,mts)

usethis::use_data(gw, overwrite=TRUE)
usethis::use_data(gw.names, overwrite=TRUE)
