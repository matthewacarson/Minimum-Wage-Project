# setwd("C:/Users/madou/OneDrive - UCLA IT Services/2)_2023_Fall/PS-170A/Minimum-Wage-Project")
library(tidyverse)
# ################################################################ #
# Importing CSVs for fast food/limited-service restaurants only ####
# ################################################################ #
limited_2011 <- 
  read_csv(
    file = "BLS/2011.q1-q4 722513 Limited-service restaurants.csv")
limited_2012 <- 
  read_csv(
    file = "BLS/2012.q1-q4 722513 Limited-service restaurants.csv")
limited_2013 <- 
  read_csv(
    file = "BLS/2013.q1-q4 722513 Limited-service restaurants.csv")
limited_2014 <- 
  read_csv(
    file = "BLS/2014.q1-q4 722513 Limited-service restaurants.csv")
limited_2015 <- 
  read_csv(
    file = "BLS/2015.q1-q4 722513 Limited-service restaurants.csv")
limited_2016 <- 
  read_csv(
    file = "BLS/2016.q1-q4 722513 NAICS 722513 Limited-service restaurants.csv")
limited_2017 <- 
  read_csv(
    file = "BLS/2017.q1-q4 722513 NAICS 722513 Limited-service restaurants.csv")
limited_2018 <- 
  read_csv(
    file = "BLS/2018.q1-q4 722513 NAICS 722513 Limited-service restaurants.csv")
limited_2019 <- 
  read_csv(
    file = "BLS/2019.q1-q4 722513 NAICS 722513 Limited-service restaurants.csv")
limited_2020 <- 
  read_csv(
    file = "BLS/2020.q1-q4 722513 NAICS 722513 Limited-service restaurants.csv")
limited_2021 <- 
  read_csv(
    file = "BLS/2021.q1-q4 722513 NAICS 722513 Limited-service restaurants.csv")
limited_2022 <- 
  read_csv(
    file = "BLS/2022.q1-q4 722513 NAICS 722513 Limited-service restaurants.csv")
limited_2023 <- 
  read_csv(
    file = "BLS/2023.q1-q1 722513 NAICS 722513 Limited-service restaurants.csv")

counties_of_interest <- "((Wyandotte|Johnson|Leavenworth|Atchison|Bourbon|Crawford|Cherokee|Brown) County, Kansas|(Buchanan|Platte|Clay|Jackson|Cass|Bates|Vernon|Barton|Jasper|Newton) County, Missouri)"

columns_of_interest <- c(
  "area_fips", "year", "qtr", "area_title", 
  # "qtrly_estabs_count", 
  "month1_emplvl", "month2_emplvl", 
  "month3_emplvl", "industry_code", "own_title"
)
## ############################################# #
## Filtering counties and columns of interest ####
## ############################################# #
limited_2011_f <- limited_2011[
  str_detect(
    string = limited_2011$area_title,
    pattern = counties_of_interest
  ), columns_of_interest
]

limited_2012_f <- limited_2012[
  str_detect(
    string = limited_2012$area_title,
    pattern = counties_of_interest
  ), columns_of_interest
]

limited_2013_f <- limited_2013[
  str_detect(
    string = limited_2013$area_title,
    pattern = counties_of_interest
  ), columns_of_interest
]

limited_2014_f <- limited_2014[
  str_detect(
    string = limited_2014$area_title,
    pattern = counties_of_interest
  ), columns_of_interest
]

limited_2015_f <- limited_2015[
  str_detect(
    string = limited_2015$area_title,
    pattern = counties_of_interest
  ), columns_of_interest
]

limited_2016_f <- limited_2016[
  str_detect(
    string = limited_2016$area_title,
    pattern = counties_of_interest
  ), columns_of_interest
]

limited_2017_f <- limited_2017[
  str_detect(
    string = limited_2017$area_title,
    pattern = counties_of_interest
  ), columns_of_interest
]

limited_2018_f <- limited_2018[
  str_detect(
    string = limited_2018$area_title,
    pattern = counties_of_interest
  ), columns_of_interest
]

limited_2019_f <- limited_2019[
  str_detect(
    string = limited_2019$area_title,
    pattern = counties_of_interest
  ), columns_of_interest
]

limited_2020_f <- limited_2020[
  str_detect(
    string = limited_2020$area_title,
    pattern = counties_of_interest
  ), columns_of_interest
]

limited_2021_f <- limited_2021[
  str_detect(
    string = limited_2021$area_title,
    pattern = counties_of_interest
  ), columns_of_interest
]

limited_2022_f <- limited_2022[
  str_detect(
    string = limited_2022$area_title,
    pattern = counties_of_interest
  ), columns_of_interest
]

limited_2023_f <- limited_2023[
  str_detect(
    string = limited_2023$area_title,
    pattern = counties_of_interest
  ), columns_of_interest
]

limited_combined <- 
  limited_2011_f |> 
  add_row(limited_2012_f) |> 
  add_row(limited_2013_f) |> 
  add_row(limited_2014_f) |> 
  add_row(limited_2015_f) |> 
  add_row(limited_2016_f) |> 
  add_row(limited_2017_f) |> 
  add_row(limited_2018_f) |> 
  add_row(limited_2019_f) |> 
  add_row(limited_2020_f) |> 
  add_row(limited_2021_f) |> 
  add_row(limited_2022_f) |> 
  add_row(limited_2023_f)

limited_combined <- limited_combined[-which(0 == limited_combined, arr.ind = T)[1:12,1],]

limited_combined$area_fips <- as.numeric(limited_combined$area_fips)

### ################################################################### #
### Gathering monthly emplvl into one column and month into another. ####
### ################################################################### #
limited_combined_gather <- 
  limited_combined |> 
  gather(
    key = "month", 
    value = "emplvl",  
    month1_emplvl, 
    month2_emplvl, 
    month3_emplvl
  ) |> 
  # Extract the numeric part from the "month" column
  mutate(
    month =
      case_when( # Converting to numeric values
        month == "month1_emplvl" ~ 1,
        month == "month2_emplvl" ~ 2,
        month == "month3_emplvl" ~ 3))

### ############################### #
### Arrange columns before pivot ####
### ############################### #
limited_combined_arrange <- limited_combined_gather |> 
  select(area_fips, area_title, year, qtr, month, emplvl) |> 
  arrange(area_title, year, qtr)

### ##################################################### #
### Pivot_wider to prepare for looping through columns ####
### ##################################################### #
limited_combined_pivot <- 
  limited_combined_arrange |>  
  pivot_wider(
    id_cols = c("area_fips", "area_title"),
    names_from = c("year", "qtr", "month"),
    values_from = c(
      # "qtrly_estabs_count", 
      "emplvl",
    ),
    names_sep = "_"
  )
#### ############################################# #
#### Changing column names to continuous values ####
#### This will be helpful for plotting later ##### #
#### ############################################# #
limited_serv_colnames <- colnames(limited_combined_pivot)[3:149]
d1 <- as.numeric(str_split_i(string = limited_serv_colnames, pattern = "_", i = 1))
d2 <- as.numeric(str_split_i(string = limited_serv_colnames, pattern = "_", i = 2))
d3 <- as.numeric(str_split_i(string = limited_serv_colnames, pattern = "_", i = 3))
limited_serv_converted <- d1 + (d2 - 1) * (1/4) + (d3 - 1) * (1/12)

# Testing to make sure all intervals are consistent.
round(diff(limited_serv_converted), digits = 5) |> table()

# ##################################################### #
# Begin importing CSVs for emp lvl in ALL industries ####
# ##################################################### #
All_Ind_2023 <- read_csv("BLS/2023.q1-q1 10 10 Total, all industries.csv")
All_Ind_2022 <- read_csv("BLS/2022.q1-q4 10 10 Total, all industries.csv")
All_Ind_2021 <- read_csv("BLS/2021.q1-q4 10 10 Total, all industries.csv")
All_Ind_2020 <- read_csv("BLS/2020.q1-q4 10 10 Total, all industries.csv")
All_Ind_2019 <- read_csv("BLS/2019.q1-q4 10 10 Total, all industries.csv")
All_Ind_2018 <- read_csv("BLS/2018.q1-q4 10 10 Total, all industries.csv")
All_Ind_2017 <- read_csv("BLS/2017.q1-q4 10 10 Total, all industries.csv")
All_Ind_2016 <- read_csv("BLS/2016.q1-q4 10 10 Total, all industries.csv")
All_Ind_2015 <- read_csv("BLS/2015.q1-q4 10 Total, all industries.csv")
All_Ind_2014 <- read_csv("BLS/2014.q1-q4 10 Total, all industries.csv")
All_Ind_2013 <- read_csv("BLS/2013.q1-q4 10 Total, all industries.csv")
All_Ind_2012 <- read_csv("BLS/2012.q1-q4 10 Total, all industries.csv")
All_Ind_2011 <- read_csv("BLS/2011.q1-q4 10 Total, all industries.csv")

## ############################################# #
## Filtering columns and counties of interest ####
## ############################################# #
All_Ind_2023_f <- All_Ind_2023[
  str_detect(
    string = All_Ind_2023$area_title,
    pattern = counties_of_interest
  ), columns_of_interest
]

All_Ind_2022_f <- All_Ind_2022[
  str_detect(
    string = All_Ind_2022$area_title,
    pattern = counties_of_interest
  ), columns_of_interest
]

All_Ind_2021_f <- All_Ind_2021[
  str_detect(
    string = All_Ind_2021$area_title,
    pattern = counties_of_interest
  ), columns_of_interest
]

All_Ind_2020_f <- All_Ind_2020[
  str_detect(
    string = All_Ind_2020$area_title,
    pattern = counties_of_interest
  ), columns_of_interest
]

All_Ind_2019_f <- All_Ind_2019[
  str_detect(
    string = All_Ind_2019$area_title,
    pattern = counties_of_interest
  ), columns_of_interest
]

All_Ind_2018_f <- All_Ind_2018[
  str_detect(
    string = All_Ind_2018$area_title,
    pattern = counties_of_interest
  ), columns_of_interest
]

All_Ind_2017_f <- All_Ind_2017[
  str_detect(
    string = All_Ind_2017$area_title,
    pattern = counties_of_interest
  ), columns_of_interest
]

All_Ind_2016_f <- All_Ind_2016[
  str_detect(
    string = All_Ind_2016$area_title,
    pattern = counties_of_interest
  ), columns_of_interest
]

All_Ind_2015_f <- All_Ind_2015[
  str_detect(
    string = All_Ind_2015$area_title,
    pattern = counties_of_interest
  ), columns_of_interest
]

All_Ind_2014_f <- All_Ind_2014[
  str_detect(
    string = All_Ind_2014$area_title,
    pattern = counties_of_interest
  ), columns_of_interest
]

All_Ind_2013_f <- All_Ind_2013[
  str_detect(
    string = All_Ind_2013$area_title,
    pattern = counties_of_interest
  ), columns_of_interest
]

All_Ind_2012_f <- All_Ind_2012[
  str_detect(
    string = All_Ind_2012$area_title,
    pattern = counties_of_interest
  ), columns_of_interest
]

All_Ind_2011_f <- All_Ind_2011[
  str_detect(
    string = All_Ind_2011$area_title,
    pattern = counties_of_interest
  ), columns_of_interest
]

all_ind_empl_filter <- 
  All_Ind_2011_f |> 
  add_row(All_Ind_2012_f) |> 
  add_row(All_Ind_2013_f) |> 
  add_row(All_Ind_2014_f) |> 
  add_row(All_Ind_2015_f) |> 
  add_row(All_Ind_2016_f) |> 
  add_row(All_Ind_2017_f) |> 
  add_row(All_Ind_2018_f) |> 
  add_row(All_Ind_2019_f) |> 
  add_row(All_Ind_2020_f) |> 
  add_row(All_Ind_2021_f) |> 
  add_row(All_Ind_2022_f) |> 
  add_row(All_Ind_2023_f) |> 
  filter(own_title == "Private")

### ######################################## #
### Gather for all industries ################ 
### Puts month and emplvl in separate columns 
### ######################################## #
all_ind_emp_gather <- 
  all_ind_empl_filter |> 
  gather(
    key = "month", 
    value = "emplvl",  
    month1_emplvl, 
    month2_emplvl, 
    month3_emplvl
  ) |> 
  # Extract the numeric part from the "month" column
  mutate(
    month =
      case_when( # Converting to numeric values
        month == "month1_emplvl" ~ 1,
        month == "month2_emplvl" ~ 2,
        month == "month3_emplvl" ~ 3))

### ############################################ #
### Arrange in preparation for pivoting wider ####
### ############################################ #
all_ind_empl_arrange <- 
  all_ind_emp_gather |> 
  select(area_fips, area_title, year, qtr, month, emplvl) |> 
  arrange(month) |> 
  arrange(qtr) |> 
  arrange(area_title) |> 
  arrange(year)

### ################################## #
### Pivot wider to prepare for loop ####
### ################################## #
all_ind_empl_pivot <- 
  all_ind_empl_arrange |>  
pivot_wider(
  id_cols = c("area_fips", "area_title"),
  names_from = c("year", "qtr", "month"),
  values_from = "emplvl",
  names_sep = "_"
)

#### ############################################# #
#### Changing column names to continuous values ####
#### This will be helpful for plotting later ##### #
#### ############################################# #
all_ind_colnames <- colnames(all_ind_empl_pivot)[3:149]
a1 <- as.numeric(str_split_i(string = all_ind_colnames, pattern = "_", i = 1))
a2 <- as.numeric(str_split_i(string = all_ind_colnames, pattern = "_", i = 2))
a3 <- as.numeric(str_split_i(string = all_ind_colnames, pattern = "_", i = 3))
all_ind_converted <- a1 + (a2 - 1) * (1/4) + (a3 - 1) * (1/12)

# Testing to make sure all intervals are consistent.
round(diff(all_ind_converted), digits = 5) |> table()

# Testing that columns as the same across both data frames
identical(all_ind_converted, limited_serv_converted)

# Diagnostic tests/not using
# table(all_ind_empl_filter$area_title, all_ind_empl_filter$year, all_ind_empl_filter$qtr)
# 
# all_ind_empl_filter %>%
#   count(area_title, year, qtr) %>%
#   arrange(desc(n))
#   
limited_combined_pivot[,1] == all_ind_empl_pivot[,1]
limited_combined_pivot[,2] == all_ind_empl_pivot[,2]

calculations_df <- data.frame(
  # area_fips = all_ind_empl_pivot$area_fips,
  area_title = all_ind_empl_pivot$area_title,
  pchg_2011_1_1 = rep(0,18)
)
# Calculate percent change for each pair of columns and create new columns for the results
prop_2011 <- limited_combined_pivot[[3]] / all_ind_empl_pivot[[3]]


  # Making sure that column names and column length are the same
if (identical(colnames(limited_combined_pivot), colnames(all_ind_empl_pivot))) {
  for (i in 3:(ncol(limited_combined_pivot) - 3)) {
    col_name <- colnames(limited_combined_pivot)[i + 1]
    new_col_name <- paste0("pchg_", col_name)
    newer_prop <- limited_combined_pivot[[i + 3]] / all_ind_empl_pivot[[i + 3]]
      # newer_prop - prop_2011 = increase/decrease since 2011
    calculations_df[[new_col_name]] <- (newer_prop - prop_2011) #* 100
  }
}

# The new columns with percent change values have been added to the dataframe

transpose <- calculations_df |> t() |> data.frame()
colnames(transpose) <- transpose[1,]
transpose <- transpose[-1,]
transpose <- cbind(
  Year = str_split_i(string = rownames(transpose), pattern = "_", i = 2) |> as.numeric() + 
    (str_split_i(string = rownames(transpose), pattern = "_", i = 3) |> as.numeric() - 1) * (1/4) +
    (str_split_i(string = rownames(transpose), pattern = "_", i = 4) |> as.numeric() - 1) * (1/12)
    ,
  transpose
)


transpose_gather <- transpose |> gather(key = "County", value = "Employment_Change", -Year)
transpose_gather$Employment_Change <- as.numeric(transpose_gather$Employment_Change)

transpose_gather$State <- transpose_gather$County |> str_split_i(pattern = ", ", i = 2)



#################################### #
# Begin Analysis ####
#################################### #

ggplot(data = transpose_gather, 
       aes(x = Year, y = Employment_Change, col = State)) +
  geom_point() +
  geom_vline(xintercept = 2019, col = 'green4', lwd = 0.9) +
  geom_smooth(
    method = "lm", 
    formula = y ~ x, 
    data = transpose_gather |> 
      filter(State == "Missouri"),
    # color = 'blue4', 
    linetype = "solid",
    se = T,
    # aes(color = "MO Overall")
  ) +
  geom_smooth(
    method = "lm", 
    formula = y ~ x, 
    data = transpose_gather |> 
      filter(State == "Missouri" & Year >= 2019),
    color = 'black',
    linetype = "dashed",
    se = T,
    # aes(color = "MO After")
  ) +
  geom_smooth(
    method = "lm", 
    formula = y ~ x, 
    data = transpose_gather |> 
      filter(State == "Kansas"),
    # color = 'red3', 
    linetype = "solid",
    se = T,
    # aes(color = "KS")
  ) +
  labs(
    title = "Limited-Service Food Industry Employment",
    subtitle = "Change as a proportion of the total employed population",
    x = "Year",
    y = "Percentile Point Change Since 2011",
    fill = "State") + 
  theme_light()

  # scale_colour_manual(
  #   name = "States", 
  #   values = c(2, 4)
  # )

# LM Models ####

lm_missouri_before <- lm(
  data = transpose_gather |> 
    filter(State == "Missouri" & Year < 2019),
  formula = Employment_Change ~ Year
)


lm_missouri_after <- lm(
  data = transpose_gather |> 
    filter(State == "Missouri" & Year >= 2019),
  formula = Employment_Change ~ Year
)

lm_missouri_overall <- lm(
  data = transpose_gather |> 
    filter(State == "Missouri"),
  formula = Employment_Change ~ Year
)

lm_kansas_before <- lm(
  data = transpose_gather |> 
    filter(State == "Kansas" & Year < 2019),
  formula = Employment_Change ~ Year
)


lm_kansas_after <- lm(
  data = transpose_gather |> 
    filter(State == "Kansas" & Year >= 2019),
  formula = Employment_Change ~ Year
)

lm_kansas_overall <- lm(
  data = transpose_gather |> 
    filter(State == "Kansas"),
  formula = Employment_Change ~ Year
)

summary(lm_missouri_before)
summary(lm_kansas_before)

lm_missouri_before$coefficients[2] - lm_kansas_before$coefficients[2]


library(sandwich)
library(lmtest)

# Perform a test for the difference in slopes
coeftest(lm_missouri_before, vcov = vcovHC(lm_kansas_before))




# ######################################################################### #
# ######################################################################### #
# ################ NOT USING CODE BELOW AS OF 11/14 ####################### #
# ######################################################################### #
# ######################################################################### #

ggplot(data = transpose_gather, 
       aes(x = Year, y = Employment_Change, col = State)) +
  geom_point() +
  geom_vline(xintercept = 2019, col = 'green3', lwd = 0.9) +
  geom_smooth(
    method = "lm", 
    formula = y ~ splines::ns(x, df = 10),
    # formula = y ~ x, 
    data = transpose_gather |> 
      filter(State == "Missouri"),
    color = 'blue2', 
    se = F
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ splines::ns(x, df = 10),
    # formula = y ~ x, 
    data = transpose_gather |> 
      filter(State == "Kansas"),
    color = 'red3', 
    se = F
  ) #+
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
  # filter(Employment_Change > -90)






## Missouri ####
#################################### #
treatment_year <- 2019
# Subset for MO
# 


MO_sub <- transpose_plot[str_detect(string = transpose_plot$County, pattern = "Missouri"),]

MO_Before_2019 <- MO_sub |> filter(Year < treatment_year)


MO_After_2019 <- MO_sub |> filter(Year >= treatment_year)



plot(
  x = MO_sub$Year,
  y = MO_sub$Employment_Change,
  xlab = "Year",
  ylab = "Employment level change since 2011 (%)",
  ylim = c(-70, 80),
  las = 1,
  main = "Missouri"
)

abline(lm_missouri_before, col = 'red', lwd = 2) # Before Min Wage Increase
abline(lm_missouri_after, col = 'blue', lwd = 2) # After
# 
# Creating manually
line_difference <- predict(lm_missouri_before, newdata = data.frame(Year = treatment_year)) - predict(lm_missouri_after, newdata = data.frame(Year = treatment_year))

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

abline(
  a = coef(lm_missouri_after)[1] + line_difference,
  b = coef(lm_missouri_after)[2],
  col = 'blue',
  lwd = 2
)

abline(v = treatment_year, col = 'orange', lwd = 2) # Min wage Increase

legend(
  "bottomleft", # Position of the legend on the plot
  legend = c("Regression Line (Before)", "Regression Line (After)", "Min Wage Increase"),
  col = c("red", "blue", "orange"), # Colors corresponding to the lines
  lwd = 2, # Line width
  # pch = 16, # Point type
  title = "Legend" # Legend title
)


abline(lm_missouri_overall, col = 'green', lwd = 2)

# Plotting after 2018 only

ggplot(data = MO_sub, aes(x = Year, y = Employment_Change)) +
  # Add a line for Employment Change
  geom_point(color = 'black') +
  geom_smooth(method = "lm", formula = y ~ x, data = MO_Before_2019, color = "blue", se = TRUE) +
  geom_smooth(method = "lm", formula = y ~ x, data = MO_After_2019, color = "red", se = TRUE) +
  labs(
    title = "MO"
  ) + geom_vline(xintercept = 2019)

############################## #
# Begin Kansas Analysis ####
############################## #

KS_sub <- transpose_plot[str_detect(string = transpose_plot$County, pattern = "Kansas"),]
KS_sub$Employment_Change[is.infinite(KS_sub$Employment_Change)] <- 100

KS_Before_2019 <- KS_sub[KS_sub$Year < treatment_year,]

KS_After_2019 <- KS_sub[KS_sub$Year >= treatment_year,]


# Edit everything below before running so that it is for KS instrad of MO
# 


plot(
  x = KS_sub$Year,
  y = KS_sub$Employment_Change,
  xlab = "Year",
  ylab = "Employment level change since 2011 (%)",
  # ylim = c(-70, 80),
  las = 1,
  main = "Kansas"
)

abline(lm_kansas_before, col = 'red', lwd = 2) # Before Min Wage Increase
abline(lm_kansas_after, col = 'green', lwd = 2) # After
# 
# Creating manually
line_difference <- predict(lm_kansas_before, newdata = data.frame(Year = treatment_year)) - predict(lm_kansas_after, newdata = data.frame(Year = treatment_year))

##################
# Segments aren't plotting right
##################

# segments(
#   x0 = treatment_year,
#   y0 = predict(lm_kansas_before, newdata = data.frame(Year = treatment_year)),
#   x1 = 2025,
#   y1 = predict(lm_kansas_after, newdata = data.frame(Year = max(KS_After_2019$Year))) + line_difference,
#   col = 'blue',
#   lwd = 2,
#   lty = 2
# )

abline(
  a = coef(lm_kansas_after)[1] + line_difference,
  b = coef(lm_kansas_after)[2],
  col = 'blue',
  lwd = 2
)

abline(v = treatment_year, col = 'orange', lwd = 2) # Min wage Increase

legend(
  "bottomleft", # Position of the legend on the plot
  legend = c("Regression Line (Before)", "Regression Line (After)", "Min Wage Increase"),
  col = c("red", "blue", "orange"), # Colors corresponding to the lines
  lwd = 2, # Line width
  # pch = 16, # Point type
  title = "Legend" # Legend title
)

