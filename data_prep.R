# ============================================================
# prep_data.R  — Run this ONCE before rendering index.qmd
# ============================================================

# tidyverse = a bundle of packages for cleaning + reshaping data
# lubridate = makes working with dates much easier
library(tidyverse)
library(lubridate)

# ── Load raw files ────────────────────────────────────────────
cycles   <- read_csv("data/physiological_cycles.csv")
workouts <- read_csv("data/workouts.csv")

# ── Clean physiological_cycles ────────────────────────────────
# This table has one row per "cycle" (roughly one day).
# It contains your recovery score, HRV, sleep info, etc.
cycles_clean <- cycles |>
  rename(
    cycle_start  = `Cycle start time`,
    recovery     = `Recovery score %`,
    hrv          = `Heart rate variability (ms)`,
    rhr          = `Resting heart rate (bpm)`,
    day_strain   = `Day Strain`,
    skin_temp    = `Skin temp (celsius)`,
    blood_ox     = `Blood oxygen %`,
    sleep_perf   = `Sleep performance %`,
    asleep_min   = `Asleep duration (min)`,
    deep_min     = `Deep (SWS) duration (min)`,
    rem_min      = `REM duration (min)`,
    light_min    = `Light sleep duration (min)`,
    sleep_debt   = `Sleep debt (min)`,
    sleep_eff    = `Sleep efficiency %`,
    resp_rate    = `Respiratory rate (rpm)`
  ) |>
  mutate(
    # Pull just the date part (drop the time)
    date = as_date(cycle_start),
    
    # Convert sleep minutes to hours (easier to think about)
    asleep_hrs     = asleep_min / 60,
    
    # What percent of your sleep was deep / REM?
    deep_pct       = deep_min  / asleep_min * 100,
    rem_pct        = rem_min   / asleep_min * 100,
    
    # Sleep debt in hours
    sleep_debt_hrs = sleep_debt / 60,
    
    # WHOOP's traffic-light zones: green / yellow / red
    recovery_zone = case_when(
      recovery >= 67 ~ "Green",
      recovery >= 34 ~ "Yellow",
      !is.na(recovery) ~ "Red"
    ),
    # factor() keeps the zones in the right order for charts
    recovery_zone = factor(recovery_zone,
                           levels = c("Red", "Yellow", "Green"))
  ) |>
  # Drop rows where recovery hasn't been calculated yet
  filter(!is.na(recovery)) |>
  arrange(date)

# ── Clean workouts ────────────────────────────────────────────
# Multiple workouts can happen in the same cycle, so this table
# has more rows than cycles_clean.
workouts_clean <- workouts |>
  rename(
    cycle_start  = `Cycle start time`,
    activity     = `Activity name`,
    strain       = `Activity Strain`,
    duration_min = `Duration (min)`,
    avg_hr       = `Average HR (bpm)`,
    max_hr       = `Max HR (bpm)`,
    calories     = `Energy burned (cal)`,
    hr_z3        = `HR Zone 3 %`,
    hr_z4        = `HR Zone 4 %`,
    hr_z5        = `HR Zone 5 %`
  ) |>
  mutate(
    date = as_date(cycle_start),
    
    # Group activities into cleaner categories.
    # Cycling (3), Basketball (6), and Dance (8) have too few
    # sessions to draw conclusions — binned into Other.
    # Threshold: fewer than 20 sessions = Other.
    activity_clean = case_when(
      activity == "Running"       ~ "Running",
      activity == "Weightlifting" ~ "Weightlifting",
      activity == "Powerlifting"  ~ "Powerlifting",
      activity == "Jiu Jitsu"     ~ "Jiu Jitsu",
      activity == "Walking"       ~ "Walking",
      activity == "Activity"      ~ "General Activity",
      TRUE                        ~ "Other"
    )
  ) |>
  filter(!is.na(avg_hr))  # drop rows with no heart rate data

# ── Join: attach the hardest workout of each day to cycles ────
# We pick the highest-strain workout per day.
# left_join() keeps ALL rows from cycles_clean, even days with no workout.
top_workouts <- workouts_clean |>
  group_by(date) |>
  slice_max(strain, n = 1, with_ties = FALSE) |>  # keep hardest workout
  ungroup() |>
  select(date, activity_clean, strain, avg_hr, max_hr, duration_min)

main_df <- cycles_clean |>
  left_join(top_workouts, by = "date")

# ── Save ──────────────────────────────────────────────────────
# saveRDS() saves R objects in a compact format — faster than CSV
saveRDS(main_df,        "data/main_df.rds")
saveRDS(workouts_clean, "data/workouts_clean.rds")

# Quick check — you should see ~369 days and ~447 workouts
cat("✅ Done!\n")
cat("Days tracked:", nrow(main_df), "\n")
cat("Workouts logged:", nrow(workouts_clean), "\n")
cat("Date range:", as.character(min(main_df$date)),
    "→", as.character(max(main_df$date)), "\n")

