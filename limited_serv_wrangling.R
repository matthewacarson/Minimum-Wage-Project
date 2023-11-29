# setwd("C:/Users/madou/OneDrive - UCLA IT Services/2)_2023_Fall/PS-170A/Minimum-Wage-Project")
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
# ################################################################ #
# Importing CSVs for fast food/limited-service restaurants only ####
# ################################################################ #
# limited_2011 <-
#   read_csv(
#     file = "BLS/2011.q1-q4 722513 Limited-service restaurants.csv")
# limited_2012 <-
#   read_csv(
#     file = "BLS/2012.q1-q4 722513 Limited-service restaurants.csv")
# limited_2013 <-
#   read_csv(
#     file = "BLS/2013.q1-q4 722513 Limited-service restaurants.csv")
# limited_2014 <-
#   read_csv(
#     file = "BLS/2014.q1-q4 722513 Limited-service restaurants.csv")
# limited_2015 <-
#   read_csv(
#     file = "BLS/2015.q1-q4 722513 Limited-service restaurants.csv")
# limited_2016 <-
#   read_csv(
#     file = "BLS/2016.q1-q4 722513 NAICS 722513 Limited-service restaurants.csv")
# limited_2017 <-
#   read_csv(
#     file = "BLS/2017.q1-q4 722513 NAICS 722513 Limited-service restaurants.csv")
# limited_2018 <-
#   read_csv(
#     file = "BLS/2018.q1-q4 722513 NAICS 722513 Limited-service restaurants.csv")
# limited_2019 <-
#   read_csv(
#     file = "BLS/2019.q1-q4 722513 NAICS 722513 Limited-service restaurants.csv")
# limited_2020 <-
#   read_csv(
#     file = "BLS/2020.q1-q4 722513 NAICS 722513 Limited-service restaurants.csv")
# limited_2021 <-
#   read_csv(
#     file = "BLS/2021.q1-q4 722513 NAICS 722513 Limited-service restaurants.csv")
# limited_2022 <-
#   read_csv(
#     file = "BLS/2022.q1-q4 722513 NAICS 722513 Limited-service restaurants.csv")
# limited_2023 <-
#   read_csv(
#     file = "BLS/2023.q1-q1 722513 NAICS 722513 Limited-service restaurants.csv")

# All the "Limited-service restaurants" CSVs can be 
# loaded with the .RData file below:
load(file = "BLS_limited-service_rest.RData", 
    envir = limited_serv_orig <- new.env())

counties_of_interest <- "((Wyandotte|Johnson|Leavenworth|Atchison|Bourbon|Cherokee|Brown) County, Kansas|(Buchanan|Platte|Clay|Jackson|Cass|Bates|Barton|Jasper|Newton|Vernon) County, Missouri)" # Removed Linn & Miami & Donaphin
# counties_of_interest <- "Kansas|Missouri"


columns_of_interest <- c(
  "area_fips", "year", "qtr", "area_title", 
  # "qtrly_estabs_count", 
  "month1_emplvl", "month2_emplvl", "month3_emplvl",  "total_qtrly_wages", 
  "taxable_qtrly_wages", "qtrly_contributions", "avg_wkly_wage", 
  "industry_code", "own_title")
## ############################################# #
## Filtering counties and columns of interest ####
## ############################################# #

limited_serv_filtered <- new.env()
limited_serv_filtered$limited_2011_f <- limited_serv_orig$limited_2011[
  str_detect(
    string = limited_serv_orig$limited_2011$area_title,
    pattern = counties_of_interest
  ), columns_of_interest]

limited_serv_filtered$limited_2012_f <- limited_serv_orig$limited_2012[
  str_detect(
    string = limited_serv_orig$limited_2012$area_title,
    pattern = counties_of_interest
  ), columns_of_interest]

limited_serv_filtered$limited_2013_f <- limited_serv_orig$limited_2013[
  str_detect(
    string = limited_serv_orig$limited_2013$area_title,
    pattern = counties_of_interest
  ), columns_of_interest]

limited_serv_filtered$limited_2014_f <- limited_serv_orig$limited_2014[
  str_detect(
    string = limited_serv_orig$limited_2014$area_title,
    pattern = counties_of_interest
  ), columns_of_interest]

limited_serv_filtered$limited_2015_f <- limited_serv_orig$limited_2015[
  str_detect(
    string = limited_serv_orig$limited_2015$area_title,
    pattern = counties_of_interest
  ), columns_of_interest]

limited_serv_filtered$limited_2016_f <- limited_serv_orig$limited_2016[
  str_detect(
    string = limited_serv_orig$limited_2016$area_title,
    pattern = counties_of_interest
  ), columns_of_interest]

limited_serv_filtered$limited_2017_f <- limited_serv_orig$limited_2017[
  str_detect(
    string = limited_serv_orig$limited_2017$area_title,
    pattern = counties_of_interest
  ), columns_of_interest]

limited_serv_filtered$limited_2018_f <- limited_serv_orig$limited_2018[
  str_detect(
    string = limited_serv_orig$limited_2018$area_title,
    pattern = counties_of_interest
  ), columns_of_interest]

limited_serv_filtered$limited_2019_f <- limited_serv_orig$limited_2019[
  str_detect(
    string = limited_serv_orig$limited_2019$area_title,
    pattern = counties_of_interest
  ), columns_of_interest]

limited_serv_filtered$limited_2020_f <- limited_serv_orig$limited_2020[
  str_detect(
    string = limited_serv_orig$limited_2020$area_title,
    pattern = counties_of_interest
  ), columns_of_interest]

limited_serv_filtered$limited_2021_f <- limited_serv_orig$limited_2021[
  str_detect(
    string = limited_serv_orig$limited_2021$area_title,
    pattern = counties_of_interest
  ), columns_of_interest]

limited_serv_filtered$limited_2022_f <- limited_serv_orig$limited_2022[
  str_detect(
    string = limited_serv_orig$limited_2022$area_title,
    pattern = counties_of_interest
  ), columns_of_interest]

limited_serv_filtered$limited_2023_f <- limited_serv_orig$limited_2023[
  str_detect(
    string = limited_serv_orig$limited_2023$area_title,
    pattern = counties_of_interest
  ), columns_of_interest]
# ####################################################### #
## Combining all data frames for all years of interest ####
# Limited Food Service ################################## #
# ####################################################### #
limited_combined <- new.env()
limited_combined$combined <- 
  limited_serv_filtered$limited_2011_f |> 
  add_row(limited_serv_filtered$limited_2012_f) |> 
  add_row(limited_serv_filtered$limited_2013_f) |> 
  add_row(limited_serv_filtered$limited_2014_f) |> 
  add_row(limited_serv_filtered$limited_2015_f) |> 
  add_row(limited_serv_filtered$limited_2016_f) |> 
  add_row(limited_serv_filtered$limited_2017_f) |> 
  add_row(limited_serv_filtered$limited_2018_f) |> 
  add_row(limited_serv_filtered$limited_2019_f) |> 
  add_row(limited_serv_filtered$limited_2020_f) |> 
  add_row(limited_serv_filtered$limited_2021_f) |> 
  add_row(limited_serv_filtered$limited_2022_f) |> 
  add_row(limited_serv_filtered$limited_2023_f)

### ################################################################### #
### Gathering monthly emplvl into one column and month into another. ####
### ################################################################### #
limited_combined$gather <- 
  limited_combined$combined |> 
  pivot_longer(
    names_to = "month", 
    cols = c(month1_emplvl, 
             month2_emplvl, 
             month3_emplvl),
    values_to = "emplvl"
  ) |> 
  # Extract the numeric part from the "month" column
  mutate(
    month =
      case_when( # Converting to numeric values
        month == "month1_emplvl" ~ 1,
        month == "month2_emplvl" ~ 2,
        month == "month3_emplvl" ~ 3))
## ############################# #
## Combine year, qtr, month ####
## ############################# #
limited_combined$gather$date <- 
  paste(
    limited_combined$gather$month + 
      (limited_combined$gather$qtr - 1) * 3, 1, 
    limited_combined$gather$year, sep = "/")

limited_combined$gather$date <- 
  as.Date(limited_combined$gather$date, format = "%m/%d/%Y")

limited_combined$converted <- 
  limited_combined$gather |> 
  mutate(
    year_decimal = (year + (qtr - 1) * (1/4) + (month - 1) * (1/12)))
  # select(-qtr, -month, -own_title)

limited_combined$pivot <- 
  limited_combined$converted |> 
  mutate(
    emplvl_limited = emplvl,
    state = case_when(
      grepl(pattern = "Missouri", x = area_title) ~ "MO",
      grepl(pattern = "Kansas", x = area_title) ~ "KS")) |> 
  select(-industry_code, -emplvl) |> 
  rename(
    ltd_total_qtrly_wages = total_qtrly_wages,
    ltd_taxable_qtrly_wages = taxable_qtrly_wages,
    ltd_qtrly_contributions = qtrly_contributions,
    ltd_avg_wkly_wage = avg_wkly_wage)
# ###################### #
# Minimum wage data ####
# ###################### #
min_wage <- new.env()
min_wage$backup <- read_csv(file = "min-wage.csv")

min_wage$filtered <-
  min_wage$backup |> 
  mutate(date = as.Date(Date, format = "%m/%d/%Y")) |> 
  filter(year(date) >= 2011) |>
  mutate(year = year(date)) |> 
  rename(state = region) |> 
  select(-Date)

min_wage$limited <- 
  left_join(
    x = limited_combined$pivot,
    y = min_wage$filtered |> select(-mw_increase, -date))

min_wage$limited_increase <-
  left_join(
    x = min_wage$limited,
    y = min_wage$filtered |> select(-mw_increase))

# min_wage$limited_increase$mw_increase[
  # is.na(min_wage$limited_increase$mw_increase)] <- 0


