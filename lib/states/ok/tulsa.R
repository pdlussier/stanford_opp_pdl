source("common.R")


# VALIDATION: [YELLOW] Tulsa's 2016 Annual Report doesn't list traffic
# statistics, but does list calls for service and arrests; these figures seem
# to be on the right order of magnitude relative to the number of calls for
# service.
load_raw <- function(raw_data_dir, n_max) {
  # NOTE: Here, "old" refers to second-wave OPP (Dan and Amy, 2018 and 2019),
  # and "new" refers to the updated data gathered by Phoebe in 2020.
  d_old <- load_regex(raw_data_dir, "^37_", n_max = n_max)
  d_new <- load_single_file(
    raw_data_dir,
    "trafficcitations_2018to06302020.csv",
    n_max = n_max
  )
  # NOTE: The new column names differ, but the contents are the same.
  d_new$data %<>%
    rename(
      CASENO = `Case Number`,
      VIOLATIONDATE = Date,
      CHARGETITLE = Title,
      CHARGESECTION = Section,
      CHARGEPARA = Paragraph,
      CHARGE = Violation,
      VIOLATION_LOCATION = Address,
      OFFICERDIV = Division
    )

  bundle_raw(
    bind_rows(d_old$data, d_new$data),
    c(d_old$loading_problems, d_new$data)
  )
}


clean <- function(d, helpers) {

  tr_race <- c(
    tr_race,
    "C" = "other",
    "D" = "other",
    "E" = "other",
    "F" = "other",
    "G" = "other",
    "K" = "other",
    "L" = "other",
    "M" = "other",
    "N" = "other",
    "P" = "other",
    "R" = "other",
    "S" = "other",
    "T" = "other",
    "V" = "other",
    "Y" = "other"
  )
  # TODO(phoebe): can we get outcome (warning, citation, arrest)?
  # https://app.asana.com/0/456927885748233/642528085814343
  # TODO(phoebe): can we get search/contraband fields?
  # https://app.asana.com/0/456927885748233/642528085814345
  # TODO(phoebe): what is CHARGEPARA and CHARGESECTION? Can we get a data
  # dictionary?
  # https://app.asana.com/0/456927885748233/642528085814346
  colnames(d$data) <- tolower(colnames(d$data))
  d$data %>%
  merge_rows(
    violationdate,
    violation_location,
    officerdiv,
    race,
    sex,
    caseno
  ) %>%
  rename(
    violation = charge,
    location = violation_location,
    speed = vehspeed,
    posted_speed = vehspeedlimit,
    vehicle_color = color,
    vehicle_make = make,
    vehicle_model = model,
    vehicle_registration_state = tagstate,
    raw_race = race,
    division = officerdiv
  ) %>%
  mutate(
    datetime = coalesce(
      parse_datetime(violationdate, "%Y/%m/%d %H:%M:%S"),
      parse_datetime(violationdate, "%Y/%m/%d")
    ),
    date = as.Date(datetime),
    time = format(datetime, "%H:%M:%S"),
    subject_race = tr_race[raw_race],
    subject_sex = tr_sex[sex],
    vehicle_year = format_two_digit_year(year),
    # NOTE: Data from 2018 on are all citations.
    outcome = if_else(year(date) >= 2018, "citation", NA_character_)
  ) %>%
  helpers$add_lat_lng(
  ) %>%
  helpers$add_shapefiles_data(
  ) %>%
  mutate(
    beat = coalesce(beat, NAME.x),
    division = DIVISION.y
  )
  debug_pipe() %>%
  helpers$add_type(
    "violation"
  ) %>%
  standardize(d$metadata)
}
