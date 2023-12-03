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
  na.omit() |>
  mutate(
    area_fips = factor(area_fips)
  )


# Creating dummies ####
# library(fastDummies)
data_2012_2013 <- joined$data |> 
  filter(year_decimal %in% seq(2012.5,2013.5, by = 1/12)) |> 
  mutate(year = as.factor(year))

# #################### #
# #######D-i-D #######
# #################### #
# models <- new.env()
# library(lfe)

################################# #
# @ Luis: Use this for each year
lm_2012_2013 <- joined$data |> 
  filter(year_decimal %in% seq(2012.5,2013.5, by = 1/12)) |> 
  mutate(year = as.factor(year)) |> 
  lm(emplvl_limited ~ year * state * emplvl_all,
       data = _)
################################# #

summary(lm_2012_2013)


# Assuming emplvl_all = 0, year = 2013, and state = "MO" in the original model
# Add other variables if your model includes more predictors
# For example, if your model includes other predictors like x1 and x2:
# new_data <- expand.grid(emplvl_all = 0, year = 2013, state = "MO", x1 = some_value, x2 = some_value)

# Use the predict function with the new data
predict(lm_2012_2013, expand.grid(emplvl_all = 0, year = "2013", state = "MO"))


# try using this with felm:  xactDOF = TRUE)

################################################# #
# Running regressions for each county for 2012-2013
# Caution! This is tentative 
################################################# #

lm_2012_2013 <-
  data_2012_2013 |> 
  lm(
    emplvl_limited ~ area_title * year * emplvl_all, 
    data = _) |> summary()# |> data.frame(coefficients, residuals, fstatistic, r_squared)

lm_2012_2013$coefficients[c("(Intercept)", "year2013", "stateMO", "year2013:stateMO")]

write.csv(x = lm_2012_2013[["coefficients"]] |> as.data.frame(), file = "each_county.csv")

################################################### #
# Creating confidence intervals for regressions
################################################### #

