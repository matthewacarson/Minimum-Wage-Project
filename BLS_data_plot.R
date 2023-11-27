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

counties_of_interest <- "((Wyandotte|Johnson|Leavenworth|Atchison|Bourbon|Cherokee|Brown|Linn|Miami|Doniphan) County, Kansas|(Buchanan|Platte|Clay|Jackson|Cass|Bates|Barton|Jasper|Newton|Vernon) County, Missouri)"
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
  gather(
    key = "month", 
    value = "emplvl",  
    month1_emplvl, 
    month2_emplvl, 
    month3_emplvl) |> 
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
    y = min_wage$filtered)

min_wage$limited_increase$mw_increase[
  is.na(min_wage$limited_increase$mw_increase)] <- 0

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
  file = "BLS_all_industries.RData",
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
  gather(
    key = "month",
    value = "emplvl",
    month1_emplvl,
    month2_emplvl,
    month3_emplvl) |>
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

# Joining all industries and limited service
joined <- new.env()

joined$data <- 
  full_join(
    x = min_wage$limited_increase,
    y = all_ind_combined$gather) |> 
  mutate(
    # year_2018 = ifelse(year(date) == 2018, 1, 0),
    proportion_limited = emplvl_limited / emplvl_all) |> 
  rename(State = state) |> 
  arrange(date) |> 
  filter(proportion_limited > 0 & proportion_limited < Inf) |> 
  mutate(proportion_limited = proportion_limited) |> 
  mutate(
    area_fips = factor(area_fips),
    area_title = factor(area_title),
    Post_Treatment = year >= 2019,
    Treatment_Group = State == "MO",
    mw_increase = min_wage - min(min_wage)) |> 
  # dummy_columns(select_columns = c("State", "year")) |> 
  na.omit()

# Filter June values

# joined$data_regression <-
  # joined$data# |>
  # filter(
    # month(date) == 6) |>  # &
      # year(date) %in% 2017:2023) |>
  # mutate(
  #   mw_increase = min_wage - 7.25
  # ) #|> 
  # na.omit()
   # mutate(month = month(date)) |>
  # select(-date, -month) |>
  # pivot_wider(
  #   names_from = c(year),
  #   names_prefix = "prop_",
  #   values_from = proportion_limited) |>

    # proportion_change = prop_2019 - prop_2018
    # ) |>
# Run panel data model with area_title as a fixed effect
# panel_model <- 
#   joined_data_1718 |> 
#   plm(proportion_limited ~  Treatment_Group + Post_Treatment+ date + area_title, 
#       model = "within",
#       index = c("area_title", "date"),
#       effect = "twoways",
#       data = _)
# 
# summary(panel_model)

# #################### #
# #######D-i-D #######
# #################### #
library(lfe)
models <- new.env()
models$felm <- 
  joined$data |> 
  lm(formula = proportion_limited ~ mw_increase + factor(date) + area_title + State)

models$felm <- 
  joined$data |> 
  lfe::felm(formula = emplvl_limited ~ min_wage + emplvl_all | area_title)#, exactDOF = TRUE)

summary(models$felm)

deci_period_prop_chg <- seq(2011.5, 2022.5, by = 1)

joined$data |> filter(year_decimal %in% deci_period_prop_chg & State == "KS") |> 
  aggregate(proportion_limited ~ State + year_decimal, FUN = mean) |> 
  arrange(State) |> 
  mutate(change = c(NA, diff(proportion_limited))) |> 
  add_row(
    joined$data |> filter(year_decimal %in% deci_period_prop_chg & State == "MO") |> 
      aggregate(proportion_limited ~ State + year_decimal, FUN = mean) |> 
      arrange(State) |> 
      mutate(change = c(NA, diff(proportion_limited)))) |> 
  pivot_wider(
    names_from = State,
    values_from = c('proportion_limited', 'change')) #|> 
  # write_csv(file = "prop_chg.csv")
  # select(-proportion_limited) |> 
  # pivot_wider(
  #   names_from = State,
  #   values_from = change
  # )
ggsave(filename = "means_with_loess.png", dpi = 'retina')
# Calculate means ####
deci_period_means_trend <- seq(2011, 2012 + 11/12, by = 1/12)

means_per_period <- joined$data |> 
  filter(year_decimal %in% deci_period_means_trend) |> 
  aggregate(proportion_limited ~ year_decimal + State, FUN = mean) |> 
  pivot_wider(
    names_from = State,
    values_from = proportion_limited
  ) |> data.frame()
diff_KS <- diff(means_per_period$KS)
diff_MO <- diff(means_per_period$MO)

lm_diffs <- lm(diff_MO ~ diff_KS)
plot(
  x = diff_KS,
  y = diff_MO,
  col = 'blue', 
  pch = 16
)
abline(lm_diffs, col = 'red')
plot(lm_diffs)

write_csv(x = means_per_period, file = "means_per_period.csv")

cor(means_per_period$KS, means_per_period$MO)

# scatter plot ####
ggplot(data = joined$data |> filter(year_decimal %in% deci_period_means_trend), 
       aes(x = year_decimal, y = proportion_limited, color = State)) +
  # geom_point(position = position_dodge(width = 0.02)) +
  geom_smooth(method = "loess", se = FALSE, span = 0.3) +
  geom_line(
    aes(group = State),
    stat = "summary",
    fun = mean,
    lwd = 1) +
  labs(title = "Means Plot") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  # stat_summary(
  #   fun = mean,
  #   geom = "point",
  #   size = 2) +
  # scale_x_continuous(
  # breaks = seq(2016, 2020, by = 0.25),
  # limits = c(2011, 2023.25)
  # ) +

# Plotting means to check for parallel trends
ggplot(data = joined$data |> filter(year_decimal %in% deci_period_means_trend), 
       aes(x = year_decimal, y = proportion_limited, color = State)) +
  geom_smooth(method = "loess", se = FALSE, span = 0.3, lwd = 0.75) +
  stat_summary(
    fun = mean,
    geom = "point") +
  geom_line(
    aes(group = State),
    stat = "summary",
    fun = mean) +
  # scale_x_continuous(
    # breaks = seq(2016, 2020, by = 0.25),
    # limits = c(2011, 2023.25)
  # ) +
  labs(title = "Means Plot") + 
  scale_y_continuous(breaks = seq(0.035, 0.05, by = 0.0025)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave(filename = "means_plot_16_19.png", dpi = 'retina')

# ################################################################### #
# ################################################################### #
# ################################################################### #
# All code below needs to be modified because of changes made above # #
# ################################################################### #
# ################################################################### #
# ################################################################### #

## Begin Regressions ####
# Regression from Eve

# lm_1 <- felm(
#   Employment ~ State + Year + Min_Wage + State:Year | as.factor(County),
#   data = transpose_gather)
# 
# lm_missouri_before <- lm(
#   data = transpose_gather |>
#     filter(State == "Missouri" & Year < 2019),
#   formula = Employment ~ Year)
# 
# lm_missouri_after <- lm(
#   data = transpose_gather |>
#     filter(State == "Missouri" & Year >= 2019),
#   formula = Employment ~ Year)
# 
# lm_missouri_overall <- lm(
#   data = transpose_gather |>
#     filter(State == "Missouri"),
#   formula = Employment ~ Year)
# 
# lm_kansas_before <- lm(
#   data = transpose_gather |>
#     filter(State == "Kansas" & Year < 2019),
#   formula = Employment ~ Year)
# 
# lm_kansas_after <- lm(
#   data = transpose_gather |>
#     filter(State == "Kansas" & Year >= 2019),
#   formula = Employment ~ Year)
# 
# lm_kansas_overall <- lm(
#   data = transpose_gather |>
#     filter(State == "Kansas"),
#   formula = Employment ~ Year)
# 
# summary(lm_missouri_before)
# summary(lm_kansas_before)
# 
# # Difference in slopes
# lm_missouri_before$coefficients[2] - lm_kansas_before$coefficients[2]
# 
# # Trying out the lm model from office hour (11/15)
# 
# lm_full <- lm(
#   formula = Employment ~
#     Year +
#     Post2019 +
#     State +
#     State:Post2019 +
#     factor(County),
#   data = transpose_gather)
# 
# summary(lm_full)

# library(sandwich)
# library(lmtest)
#
# # Perform a test for the difference in slopes
# # (I don't think this is the right test.)
# coeftest(lm_full, vcov = vcovHC(lm_full))
#
# library(lsmeans)
#
# lstrends(lm_missouri_before, lm_kansas_before, var = "Year")
# confint(lm_missouri_before, parm = "Year", level = 0.9)
# confint(lm_kansas_overall, parm = "Year", level = 0.9)

#################################### #
# Begin Analysis ####
#################################### #
# coefficients_lm_missouri_before <-
#   coef(lm_missouri_before)
# # main_plot <-
# ggplot(data = transpose_gather,
#        aes(x = Year, y = Employment, col = State)) +
#   geom_point() +
#   geom_vline(aes(xintercept = 2019, linetype = "$0.8 Increase"),
#              col = 'green4', lwd = 0.9) +
#   geom_abline(
#     aes(slope = coefficients_lm_missouri_before[2],
#         intercept = coefficients_lm_missouri_before[1],
#         color = "Missouri"),
#     lwd = 0.9,
#     show.legend = F
#     ) +
#   geom_smooth(
#     method = "lm",
#     formula = y ~ x,
#     data = transpose_gather |>
#       filter(State == "Missouri" & Year >= 2019),
#     color = 'black',
#     linetype = "dashed",
#     se = F,
#     lwd = 0.9
#     # aes(color = "MO After")
#   ) +
#   geom_smooth(
#     method = "lm",
#     formula = y ~ x,
#     data = transpose_gather |>
#       filter(State == "Kansas"),
#     # color = 'red3',
#     linetype = "solid",
#     se = F,
#     lwd = 0.9,
#     # aes(color = "KS")
#   ) +
#   labs(
#     title =
#       "Limited-Service Food Industry Employment",
#     subtitle =
#       "Proportion of total employed working in limited-service restaurants.",
#     x =
#       "Year",
#     y =
#       "Proportion working in limited-service restaurants",
#     fill =
#       "State",
#     linetype = "") +
#   # scale_color_manual(values = c("Missouri" = "black", "Kansas" = "red3", "Vertical Line" = "green4")) +
#   scale_linetype_manual(values = c("$0.8 Increase" = "solid")) +
#   theme_light()
# 
# ggsave(
#   filename = "Rplot08.png",
#   plot = main_plot,
#   height = 800 * 3,
#   width = 1400 * 3,
#   units = 'px'
# )
  # geom_smooth(
  #   method = "lm",
  #   formula = y ~ x,
  #   data = transpose_gather |>
  #     filter(State == "Missouri" & Year < 2019),
  #   # color = 'blue4',
  #   linetype = "solid",
  #   se = F,
  #   # aes(color = "MO Overall")
  # ) +





# ##################################################################### #
# ##################################################################### #
# ################ NOT USING CODE BELOW AS OF 11/14 #####################
# ##################################################################### #
# ##################################################################### #

# ggplot(data = transpose_gather,
#        aes(x = Year, y = Employment, col = State)) +
#   geom_point() +
#   geom_vline(xintercept = 2019, col = 'green3', lwd = 0.9) +
#   geom_smooth(
#     method = "lm",
#     formula = y ~ splines::ns(x, df = 10),
#     # formula = y ~ x,
#     data = transpose_gather |>
#       filter(State == "Missouri"),
#     color = 'blue2',
#     se = F
#   ) +
#   geom_smooth(
#     method = "lm",
#     formula = y ~ splines::ns(x, df = 10),
#     # formula = y ~ x,
#     data = transpose_gather |>
#       filter(State == "Kansas"),
#     color = 'red3',
#     se = F
#   ) #+
  # geom_smooth(
  #   method = "loess",
  #   formula = y ~ x,
  #   data = transpose_gather |>
  #     filter(State == "Missouri"),
  #   color = 'green',
  #   se = TRUE
  # ) +
  # geom_smooth(
  #   method = "loess",
  #   formula = y ~ x,
  #   data = transpose_gather |>
  #     filter(State == "Kansas"),
  #   color = 'purple',
  #   se = TRUE
  # )

# transpose_plot$old_year <- transpose_plot$Year
# transpose_plot <- transpose_plot |>
#   mutate(
#     Year =
#       substr(transpose_plot$old_year,start = 1,stop = 4) |>
#       as.numeric() +
#       (substr(old_year, start = 5, stop = 5) |>
#          as.numeric() - 1) * 0.25 +
#       (substr(old_year, start = 6, stop = 6) |>
#          as.numeric() - 1) * (1/12)
#   )

# Filter out values < -50
# transpose_plot <- transpose_plot |>
  # filter(Employment > -90)

#################################### #
# treatment_year <- 2019
# Subset for MO

# MO_sub <- transpose_plot[str_detect(string = transpose_plot$County, pattern = "Missouri"),]
# MO_Before_2019 <- MO_sub |> filter(Year < treatment_year)
# MO_After_2019 <- MO_sub |> filter(Year >= treatment_year)
# 
# plot(
#   x = MO_sub$Year,
#   y = MO_sub$Employment,
#   xlab = "Year",
#   ylab = "Employment level change since 2011 (%)",
#   ylim = c(-70, 80),
#   las = 1,
#   main = "Missouri"
# )
# 
# abline(lm_missouri_before, col = 'red', lwd = 2) # Before Min Wage Increase
# abline(lm_missouri_after, col = 'blue', lwd = 2) # After
# #
# # Creating manually
# line_difference <- predict(lm_missouri_before, newdata = data.frame(Year = treatment_year)) - predict(lm_missouri_after, newdata = data.frame(Year = treatment_year))

################## #
# Segments aren't plotting right
################## #

# segments(
#   x0 = treatment_year,
#   y0 = predict(lm_missouri_before, newdata = data.frame(Year = treatment_year)),
#   x1 = 2025,
#   y1 = predict(lm_missouri_after, newdata = data.frame(Year = max(MO_After_2019$Year))) + line_difference,
#   col = 'blue',
#   lwd = 2,
#   lty = 2
# )

# abline(
#   a = coef(lm_missouri_after)[1] + line_difference,
#   b = coef(lm_missouri_after)[2],
#   col = 'blue',
#   lwd = 2
# )
# 
# abline(v = treatment_year, col = 'orange', lwd = 2) # Min wage Increase
# 
# legend(
#   "bottomleft", # Position of the legend on the plot
#   legend = c("Regression Line (Before)", "Regression Line (After)", "Min Wage Increase"),
#   col = c("red", "blue", "orange"), # Colors corresponding to the lines
#   lwd = 2, # Line width
#   # pch = 16, # Point type
#   title = "Legend" # Legend title
# )
# 
# 
# abline(lm_missouri_overall, col = 'green', lwd = 2)

# Plotting after 2018 only

# ggplot(data = MO_sub, aes(x = Year, y = Employment)) +
#   # Add a line for Employment Change
#   geom_point(color = 'black') +
#   geom_smooth(method = "lm", formula = y ~ x, data = MO_Before_2019, color = "blue", se = TRUE) +
#   geom_smooth(method = "lm", formula = y ~ x, data = MO_After_2019, color = "red", se = TRUE) +
#   labs(
#     title = "MO"
#   ) + geom_vline(xintercept = 2019)
# 
# ############################## #
# ############################## #
# 
# KS_sub <- transpose_plot[str_detect(string = transpose_plot$County, pattern = "Kansas"),]
# KS_sub$Employment[is.infinite(KS_sub$Employment)] <- 100
# 
# KS_Before_2019 <- KS_sub[KS_sub$Year < treatment_year,]
# 
# KS_After_2019 <- KS_sub[KS_sub$Year >= treatment_year,]
# 
# 
# # Edit everything below before running so that it is for KS instrad of MO
# #
# 
# 
# plot(
#   x = KS_sub$Year,
#   y = KS_sub$Employment,
#   xlab = "Year",
#   ylab = "Employment level change since 2011 (%)",
#   # ylim = c(-70, 80),
#   las = 1,
#   main = "Kansas"
# )
# 
# abline(lm_kansas_before, col = 'red', lwd = 2) # Before Min Wage Increase
# abline(lm_kansas_after, col = 'green', lwd = 2) # After
# #
# # Creating manually
# line_difference <- predict(lm_kansas_before, newdata = data.frame(Year = treatment_year)) - predict(lm_kansas_after, newdata = data.frame(Year = treatment_year))

################## #
# Segments aren't plotting right
################## #

# segments(
#   x0 = treatment_year,
#   y0 = predict(lm_kansas_before, newdata = data.frame(Year = treatment_year)),
#   x1 = 2025,
#   y1 = predict(lm_kansas_after, newdata = data.frame(Year = max(KS_After_2019$Year))) + line_difference,
#   col = 'blue',
#   lwd = 2,
#   lty = 2
# )

# abline(
#   a = coef(lm_kansas_after)[1] + line_difference,
#   b = coef(lm_kansas_after)[2],
#   col = 'blue',
#   lwd = 2
# )
# 
# abline(v = treatment_year, col = 'orange', lwd = 2) # Min wage Increase
# 
# legend(
#   "bottomleft", # Position of the legend on the plot
#   legend = c("Regression Line (Before)", "Regression Line (After)", "Min Wage Increase"),
#   col = c("red", "blue", "orange"), # Colors corresponding to the lines
#   lwd = 2, # Line width
#   # pch = 16, # Point type
#   title = "Legend" # Legend title
# )
# 
