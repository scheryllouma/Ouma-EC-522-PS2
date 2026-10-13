library(tidyquant)
library(tidyverse)

unemp_raw <- tq_get(
  c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
  get  = "economic.data",
  from = "1976-01-01"
)

head(unemp_raw)

dir.create("data", showWarnings = FALSE)

write_rds(unemp_raw, "data/unemp_raw.rds")


#3.2

library(tidyquant)
library(tidyverse)

# unemp_raw <- tq_get(
#   c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
#   get  = "economic.data",
#   from = "1976-01-01"
# )
# 
# head(unemp_raw)
# 
# dir.create("data", showWarnings = FALSE)
# 
# write_rds(unemp_raw, "data/unemp_raw.rds")

unemp_raw = read_rds("data/unemp_raw.rds")


# 4.1
unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUR"  ~ "Oregon",
      symbol == "CAUR"  ~ "California",
      symbol == "TXUR"  ~ "Texas",
      symbol == "NYUR"  ~ "New York"
    )
  )
# 4.2
# Fill in missing Oct 2025 with Sep 2025 value within each region
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  ) |>
  ungroup()

# Month-over-month change within each region
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(unemployment_rate_change = unemployment_rate - lag(unemployment_rate)) |>
  ungroup()

# Observations from January 2015 onward
unemp_recent <- unemp |>
  filter(date >= as.Date("2015-01-01"))

# Summary stats, whole sample
unemp_summary <- unemp |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate, na.rm = TRUE),
    max_rate  = max(unemployment_rate, na.rm = TRUE)
  )

# Summary stats, 2015 onward
unemp_recent_summary <- unemp_recent |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate, na.rm = TRUE),
    max_rate  = max(unemployment_rate, na.rm = TRUE)
  )
unemp |> filter(is.na(unemployment_rate))   # should now be 0 rows
unemp |> filter(date == as.Date("2025-10-01"))   # same value as Sep 2025 in each region
unemp_summary
unemp_recent_summary

# 4.3
unemp_top_increases <- unemp |>
  slice_max(unemployment_rate_change, n = 5)

unemp_top_decreases <- unemp |>
  slice_min(unemployment_rate_change, n = 5)

# 4.4
unemp_top_increases_no_covid <- unemp |>
  filter(year(date) != 2020) |>
  slice_max(unemployment_rate_change, n = 5)

unemp_top_decreases_no_covid <- unemp |>
  filter(year(date) != 2020) |>
  slice_min(unemployment_rate_change, n = 5)
# 5.1
ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  labs(
    title = "Monthly Unemployment Rate by Region, 1976-2026",
    x = "Date",
    y = "Unemployment Rate (%)",
    color = "Region"
  ) +
  theme_minimal()
# 5.2
ggplot(unemp, aes(x = date, y = unemployment_rate)) +
  geom_line(color = "steelblue") +
  facet_wrap(~ region) +
  labs(
    title = "Monthly Unemployment Rate by Region, 1976-2026",
    x = "Date",
    y = "Unemployment Rate (%)"
  ) +
  theme_minimal()