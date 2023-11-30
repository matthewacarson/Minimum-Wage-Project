# # ##################################################### #
# # Begin importing CSVs for emp lvl in ALL industries ####
# # ##################################################### #
# All_Ind_2023 <- read_csv("BLS/2023.q1-q1 10 10 Total, all industries.csv")
# All_Ind_2022 <- read_csv("BLS/2022.q1-q4 10 10 Total, all industries.csv")
# All_Ind_2021 <- read_csv("BLS/2021.q1-q4 10 10 Total, all industries.csv")
# All_Ind_2020 <- read_csv("BLS/2020.q1-q4 10 10 Total, all industries.csv")
# All_Ind_2019 <- read_csv("BLS/2019.q1-q4 10 10 Total, all industries.csv")
# All_Ind_2018 <- read_csv("BLS/2018.q1-q4 10 10 Total, all industries.csv")
# All_Ind_2017 <- read_csv("BLS/2017.q1-q4 10 10 Total, all industries.csv")
# All_Ind_2016 <- read_csv("BLS/2016.q1-q4 10 10 Total, all industries.csv")
# All_Ind_2015 <- read_csv("BLS/2015.q1-q4 10 Total, all industries.csv")
# All_Ind_2014 <- read_csv("BLS/2014.q1-q4 10 Total, all industries.csv")
# All_Ind_2013 <- read_csv("BLS/2013.q1-q4 10 Total, all industries.csv")
# All_Ind_2012 <- read_csv("BLS/2012.q1-q4 10 Total, all industries.csv")
# All_Ind_2011 <- read_csv("BLS/2011.q1-q4 10 Total, all industries.csv")


# All the "all industries" CSVs can be loaded with the .RData file below:
load(
  file = "RData/BLS_all_industries.RData",
  envir = all_industries_orig <- new.env())
# ## ############################################# #
# ## Filtering columns and counties of interest ####
# ## ############################################# #
all_industries_filtered <- new.env()
all_industries_filtered$All_Ind_2023_f <- all_industries_orig$All_Ind_2023[
  str_detect(
    string = all_industries_orig$All_Ind_2023$area_title,
    pattern = counties_of_interest
  ), columns_of_interest]

all_industries_filtered$All_Ind_2022_f <- all_industries_orig$All_Ind_2022[
  str_detect(
    string = all_industries_orig$All_Ind_2022$area_title,
    pattern = counties_of_interest
  ), columns_of_interest]

all_industries_filtered$All_Ind_2021_f <- all_industries_orig$All_Ind_2021[
  str_detect(
    string = all_industries_orig$All_Ind_2021$area_title,
    pattern = counties_of_interest
  ), columns_of_interest]

all_industries_filtered$All_Ind_2020_f <- all_industries_orig$All_Ind_2020[
  str_detect(
    string = all_industries_orig$All_Ind_2020$area_title,
    pattern = counties_of_interest
  ), columns_of_interest]

all_industries_filtered$All_Ind_2019_f <- all_industries_orig$All_Ind_2019[
  str_detect(
    string = all_industries_orig$All_Ind_2019$area_title,
    pattern = counties_of_interest
  ), columns_of_interest]

all_industries_filtered$All_Ind_2018_f <- all_industries_orig$All_Ind_2018[
  str_detect(
    string = all_industries_orig$All_Ind_2018$area_title,
    pattern = counties_of_interest
  ), columns_of_interest]

all_industries_filtered$All_Ind_2017_f <- all_industries_orig$All_Ind_2017[
  str_detect(
    string = all_industries_orig$All_Ind_2017$area_title,
    pattern = counties_of_interest
  ), columns_of_interest]

all_industries_filtered$All_Ind_2016_f <- all_industries_orig$All_Ind_2016[
  str_detect(
    string = all_industries_orig$All_Ind_2016$area_title,
    pattern = counties_of_interest
  ), columns_of_interest]

all_industries_filtered$All_Ind_2015_f <- all_industries_orig$All_Ind_2015[
  str_detect(
    string = all_industries_orig$All_Ind_2015$area_title,
    pattern = counties_of_interest
  ), columns_of_interest]

all_industries_filtered$All_Ind_2014_f <- all_industries_orig$All_Ind_2014[
  str_detect(
    string = all_industries_orig$All_Ind_2014$area_title,
    pattern = counties_of_interest
  ), columns_of_interest]

all_industries_filtered$All_Ind_2013_f <- all_industries_orig$All_Ind_2013[
  str_detect(
    string = all_industries_orig$All_Ind_2013$area_title,
    pattern = counties_of_interest
  ), columns_of_interest]

all_industries_filtered$All_Ind_2012_f <- all_industries_orig$All_Ind_2012[
  str_detect(
    string = all_industries_orig$All_Ind_2012$area_title,
    pattern = counties_of_interest
  ), columns_of_interest]

all_industries_filtered$All_Ind_2011_f <- all_industries_orig$All_Ind_2011[
  str_detect(
    string = all_industries_orig$All_Ind_2011$area_title,
    pattern = counties_of_interest
  ), columns_of_interest]
## ################################################# #
## Combining employment for all years of interest ####
## All employment; all industries ################## #
## ################################################# #
all_ind_combined <- new.env()
all_ind_combined$combined <-
  all_industries_filtered$All_Ind_2011_f |>
  add_row(all_industries_filtered$All_Ind_2012_f) |>
  add_row(all_industries_filtered$All_Ind_2013_f) |>
  add_row(all_industries_filtered$All_Ind_2014_f) |>
  add_row(all_industries_filtered$All_Ind_2015_f) |>
  add_row(all_industries_filtered$All_Ind_2016_f) |>
  add_row(all_industries_filtered$All_Ind_2017_f) |>
  add_row(all_industries_filtered$All_Ind_2018_f) |>
  add_row(all_industries_filtered$All_Ind_2019_f) |>
  add_row(all_industries_filtered$All_Ind_2020_f) |>
  add_row(all_industries_filtered$All_Ind_2021_f) |>
  add_row(all_industries_filtered$All_Ind_2022_f) |>
  add_row(all_industries_filtered$All_Ind_2023_f) |>
  dplyr::filter(own_title == "Private") |> 
  rename(
    all_total_qtrly_wages = total_qtrly_wages,
    all_taxable_qtrly_wages = taxable_qtrly_wages,
    all_qtrly_contributions = qtrly_contributions,
    all_avg_wkly_wage = avg_wkly_wage)

# ### ######################################## #
# ### Gather for all industries ################ 
# ### Puts month and emplvl in separate columns 
# ### ######################################## #
all_ind_combined$gather <-
  all_ind_combined$combined |>
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

all_ind_combined$gather$emplvl_all <- all_ind_combined$gather$emplvl

all_ind_combined$gather$emplvl <- NULL
all_ind_combined$gather$industry_code <- NULL