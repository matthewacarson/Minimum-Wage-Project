# Run data wrangling files first ####
# Run limited service script
source(file = "limited_serv_wrangling.R", local = wrangled <- new.env())
# Run all industries script
source(file = "all_industries_data_wrangling.R", local = wrangled)

# Joining all industries and limited service ####

joined <- new.env()
joined$data <- full_join(
  x = wrangled$min_wage$limited_increase,
  y = wrangled$all_ind_combined$gather) |> 
  mutate(
    proportion_limited = emplvl_limited / emplvl_all) |>
  # filter(proportion_limited > 0 & !is.infinite(proportion_limited)) |> 
  na.omit() |>
  mutate(
    area_fips = factor(area_fips)
  )

################################ #
# Diagnostics ####
################################ #

# Checking if there are any zeros ####
# range(joined$data$proportion_limited)
# range(joined$data$emplvl_all)
# range(joined$data$emplvl_limited)
# any(joined$data[,-22:-23] == 0)
# # Checking how many times each county appears
# table(joined$data$year, joined$data$area_title)

# write_csv(x = joined$data, file = "joined_data.csv")
# joined <- new.env()
# joined$data <- read_csv(file = "joined_data.csv")


# Creating dummies ####
library(fastDummies)
data_2012_2013 <- joined$data |> 
  filter(year %in% 2012:2013) |> 
  dummy_columns(
    select_columns = c('year', 'state'), 
    remove_first_dummy = TRUE, 
  ) |> 
  rename(post = year_2013, treat = state_MO)

# write_csv(x = data_2012_2013, file = "data_2012_2013.csv")
# data_2012_2013 <- read_csv(file = "data_2012_2013.csv")


# # Checking if there are any zeros ####
# range(data_2012_2013$proportion_limited)
# range(data_2012_2013$emplvl_all)
# range(data_2012_2013$emplvl_limited)
# any(data_2012_2013[,-22:-23] == 0)
# # Checking how many times each county appears
# table(data_2012_2013$year, data_2012_2013$area_title)

# #################### #
# #######D-i-D #######
# #################### #
# models <- new.env()
library(lfe)

felm_proportion <-
  felm(proportion_limited ~ treat * post | area_title,
       data = data_2012_2013)

# summary(felm_proportion)

felm_all <-
  felm(emplvl_limited ~ treat * post + emplvl_all | area_title,
       data = data_2012_2013)

# summary(felm_all)

felm_cont_treatment <- 
  felm(emplvl_limited ~ min_wage + emplvl_all | area_title + date, 
       data = joined$data)

# summary(felm_cont_treatment)

lm_cont_treatment <- 
  lm(emplvl_limited ~ min_wage + emplvl_all + area_title + factor(date), 
       data = joined$data)

# summary(lm_cont_treatment)

# try using this with felm:  xactDOF = TRUE)

# Trying without fixed effects ####
lm_proportion <-
  lm(proportion_limited ~ treat * post,
       data = data_2012_2013)

# summary(lm_proportion)

lm_all <-
  lm(emplvl_limited ~ treat : post + emplvl_all,
       data = data_2012_2013)

# summary(lm_all)
