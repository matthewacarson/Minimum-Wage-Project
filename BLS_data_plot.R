<<<<<<< HEAD
setwd("C:/Users/madou/OneDrive - UCLA IT Services/2)_2023_Fall/PS-170A/Minimum-Wage-Project")

library(tidyverse)

Year_2011 <- read_csv(file = "BLS/2011.q1-q4 722513 Limited-service restaurants.csv")

Year_2012 <- read_csv(file = "BLS/2012.q1-q4 722513 Limited-service restaurants.csv")

Year_2013 <- read_csv(file = "BLS/2013.q1-q4 722513 Limited-service restaurants.csv")

Year_2014 <- read_csv(file = "BLS/2014.q1-q4 722513 Limited-service restaurants.csv")

Year_2015 <- read_csv(file = "BLS/2015.q1-q4 722513 Limited-service restaurants.csv")

Year_2016 <- read_csv(file = "BLS/2016.q1-q4 722513 NAICS 722513 Limited-service restaurants.csv")

Year_2017 <- read_csv(file = "BLS/2017.q1-q4 722513 NAICS 722513 Limited-service restaurants.csv")

Year_2018 <- read_csv(file = "BLS/2018.q1-q4 722513 NAICS 722513 Limited-service restaurants.csv")

Year_2019 <- read_csv(file = "BLS/2019.q1-q4 722513 NAICS 722513 Limited-service restaurants.csv")

Year_2020 <- read_csv(file = "BLS/2020.q1-q4 722513 NAICS 722513 Limited-service restaurants.csv")

Year_2021 <- read_csv(file = "BLS/2021.q1-q4 722513 NAICS 722513 Limited-service restaurants.csv")

Year_2022 <- read_csv(file = "BLS/2022.q1-q4 722513 NAICS 722513 Limited-service restaurants.csv")

Year_2023 <- read_csv(file = "BLS/2023.q1-q1 722513 NAICS 722513 Limited-service restaurants.csv")


counties_of_interest <- "((Wyandotte|Johnson|Leavenworth|Jefferson|Doniphan|Atchison|Miami|Linn|Bourbon|Crawford|Cherokee|Brown) County, Kansas|(Holt|Andrew|Buchanan|Platte|Clay|Jackson|Cass|Bates|Vernon|Barton|Jasper|Newton) County, Missouri)"

filterd_columns <- c(
  "area_fips", "year", "qtr", "area_title", 
  "qtrly_estabs_count", "month1_emplvl", "month2_emplvl", 
  "month3_emplvl"
)

Year_2011 <- Year_2011[
  str_detect(
    string = Year_2011$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Year_2012 <- Year_2012[
  str_detect(
    string = Year_2012$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Year_2013 <- Year_2013[
  str_detect(
    string = Year_2013$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Year_2014 <- Year_2014[
  str_detect(
    string = Year_2014$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Year_2015 <- Year_2015[
  str_detect(
    string = Year_2015$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Year_2016 <- Year_2016[
  str_detect(
    string = Year_2016$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Year_2017 <- Year_2017[
  str_detect(
    string = Year_2017$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]


Year_2018 <- Year_2018[
  str_detect(
    string = Year_2018$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Year_2019 <- Year_2019[
  str_detect(
    string = Year_2019$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Year_2020 <- Year_2020[
  str_detect(
    string = Year_2020$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Year_2021 <- Year_2021[
  str_detect(
    string = Year_2021$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Year_2022 <- Year_2022[
  str_detect(
    string = Year_2022$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Year_2023 <- Year_2023[
  str_detect(
    string = Year_2023$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Combined_Years <- Year_2011 |> 
  add_row(Year_2012) |> 
  add_row(Year_2013) |> 
  add_row(Year_2014) |> 
  add_row(Year_2015) |> 
  add_row(Year_2016) |> 
  add_row(Year_2017) |> 
  add_row(Year_2018) |> 
  add_row(Year_2019) |> 
  add_row(Year_2020) |> 
  add_row(Year_2021) |> 
  add_row(Year_2022) |> 
  add_row(Year_2023)

write_csv(x = Combined_Years, file = "BLS_Combined_Years.csv")

Combined_Years_backup <- Combined_Years
# Combined_Years <- Combined_Years_backup

Combined_Years <- Combined_Years |> 
  # Gather the columns month1_emplvl, month2_emplvl, and month3_emplvl into key-value pairs
  gather(key = "month", value = "emplvl",  month1_emplvl, month2_emplvl, month3_emplvl) |> 
  # Extract the numeric part from the "month" column
  mutate(
    month =
      case_when(
        month == "month1_emplvl" ~ 1,
        month == "month2_emplvl" ~ 2,
        month == "month3_emplvl" ~ 3
      )
  )

Combined_Years <- Combined_Years |> arrange(area_title, year, qtr)


# Change the file location below to where you want to save the csv

pivot_combined <- Combined_Years |>  pivot_wider(
  id_cols = c("area_fips", "area_title"),
  names_from = c("year", "qtr", "month"),
  values_from = c(
    # "qtrly_estabs_count", 
    "emplvl",
  ),
  names_sep = ""
)

pivot_combined$`201111_pct_change` <- 0

# Calculate percent change for each pair of columns and create new columns for the results
for (i in 3:(ncol(pivot_combined) - 2)) {
  # Calculate percent change using the formula: ((new value - old value) / old value) * 100
  col_name <- colnames(pivot_combined)[i + 1]
  new_col_name <- paste0(col_name, "_pct_change")
  
  pivot_combined[[new_col_name]] <- ((pivot_combined[[i + 1]] - pivot_combined[[3]]) / pivot_combined[[3]]) * 100
}

# The new columns with percent change values have been added to the dataframe

pivot_sub <- pivot_combined[,c(2,150:296)]

transpose <- pivot_sub |> t() |> data.frame()
colnames(transpose) <- transpose[1,]
transpose <- transpose[-1,]
transpose <- cbind(
  Year = str_split(string = rownames(transpose), pattern = "_", simplify = T)[,1] |> as.numeric(),
  transpose
)
rownames(transpose) <- NULL

transpose_plot <- transpose |> gather(key = "County", value = "Employment Change", -Year)



transpose_plot$old_year <- transpose_plot$Year

transpose_plot <- transpose_plot |>
  mutate(
    Year =
      substr(transpose_plot$old_year,start = 1,stop = 4) |> 
      as.numeric() + 
      (substr(old_year, start = 5, stop = 5) |> 
         as.numeric() - 1) * 0.25 +
      (substr(old_year, start = 6, stop = 6) |> 
         as.numeric() - 1) * (1/12)
  )

# Filter out values < -50
transpose_plot <- transpose_plot |> 
  filter(`Employment Change` > -90)


MO_sub <- transpose_plot[str_detect(string = transpose_plot$County, pattern = "Missouri"),]

MO_Before_2018 <- MO_sub |> filter(Year < 2018)

MO_After_2018 <- MO_sub |> filter(Year > 2018)

lm_missouri_before <- lm(
  data = MO_Before_2018,
  formula = `Employment Change` ~ Year
)

lm_missouri_after <- lm(
  data = MO_After_2018,
  formula = `Employment Change` ~ Year
)

lm_missouri_overall <- lm(
  data = MO_sub,
  formula = `Employment Change` ~ Year
)

plot(
  x = MO_sub$Year,
  y = MO_sub$`Employment Change`,
  xlab = "Year",
  ylab = "Employment level change since 2011 (%)",
  ylim = c(-60, 80),
  las = 1
)

abline(lm_missouri_before, col = 'red', lwd = 2) # Before Min Wage Increase
abline(lm_missouri_after, col = 'green', lwd = 2) # After
abline(v = 2018, col = 'blue', lwd = 2) # Min wage Increase

legend(
  "bottomleft", # Position of the legend on the plot
  legend = c("Regression Line (Before)", "Regression Line (After)", "Min Wage Increase"),
  col = c("red", "green", "blue"), # Colors corresponding to the lines
  lwd = 2, # Line width
  # pch = 16, # Point type
  title = "Legend" # Legend title
)


abline(lm_missouri_overall, col = 'blue', lwd = 2)

# plotting before 2018 only
plot(
  x = MO_Before_2018$Year,
  y = MO_Before_2018$`Employment Change`
)

ggplot(transpose_plot) +
  aes(
    x = Year
  )
=======
setwd("C:/Users/madou/OneDrive - UCLA IT Services/2)_2023_Fall/PS-170A/Minimum-Wage-Project")

library(tidyverse)

Year_2011 <- read_csv(file = "BLS/2011.q1-q4 722513 Limited-service restaurants.csv")

Year_2012 <- read_csv(file = "BLS/2012.q1-q4 722513 Limited-service restaurants.csv")

Year_2013 <- read_csv(file = "BLS/2013.q1-q4 722513 Limited-service restaurants.csv")

Year_2014 <- read_csv(file = "BLS/2014.q1-q4 722513 Limited-service restaurants.csv")

Year_2015 <- read_csv(file = "BLS/2015.q1-q4 722513 Limited-service restaurants.csv")

Year_2016 <- read_csv(file = "BLS/2016.q1-q4 722513 NAICS 722513 Limited-service restaurants.csv")

Year_2017 <- read_csv(file = "BLS/2017.q1-q4 722513 NAICS 722513 Limited-service restaurants.csv")

Year_2018 <- read_csv(file = "BLS/2018.q1-q4 722513 NAICS 722513 Limited-service restaurants.csv")

Year_2019 <- read_csv(file = "BLS/2019.q1-q4 722513 NAICS 722513 Limited-service restaurants.csv")

Year_2020 <- read_csv(file = "BLS/2020.q1-q4 722513 NAICS 722513 Limited-service restaurants.csv")

Year_2021 <- read_csv(file = "BLS/2021.q1-q4 722513 NAICS 722513 Limited-service restaurants.csv")

Year_2022 <- read_csv(file = "BLS/2022.q1-q4 722513 NAICS 722513 Limited-service restaurants.csv")

Year_2023 <- read_csv(file = "BLS/2023.q1-q1 722513 NAICS 722513 Limited-service restaurants.csv")


counties_of_interest <- "((Wyandotte|Johnson|Leavenworth|Jefferson|Doniphan|Atchison|Miami|Linn|Bourbon|Crawford|Cherokee|Brown) County, Kansas|(Holt|Andrew|Buchanan|Platte|Clay|Jackson|Cass|Bates|Vernon|Barton|Jasper|Newton) County, Missouri)"

filterd_columns <- c(
  "area_fips", "year", "qtr", "area_title", 
  "qtrly_estabs_count", "month1_emplvl", "month2_emplvl", 
  "month3_emplvl"
)

Year_2011 <- Year_2011[
  str_detect(
    string = Year_2011$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Year_2012 <- Year_2012[
  str_detect(
    string = Year_2012$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Year_2013 <- Year_2013[
  str_detect(
    string = Year_2013$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Year_2014 <- Year_2014[
  str_detect(
    string = Year_2014$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Year_2015 <- Year_2015[
  str_detect(
    string = Year_2015$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Year_2016 <- Year_2016[
  str_detect(
    string = Year_2016$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Year_2017 <- Year_2017[
  str_detect(
    string = Year_2017$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]


Year_2018 <- Year_2018[
  str_detect(
    string = Year_2018$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Year_2019 <- Year_2019[
  str_detect(
    string = Year_2019$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Year_2020 <- Year_2020[
  str_detect(
    string = Year_2020$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Year_2021 <- Year_2021[
  str_detect(
    string = Year_2021$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Year_2022 <- Year_2022[
  str_detect(
    string = Year_2022$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Year_2023 <- Year_2023[
  str_detect(
    string = Year_2023$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Combined_Years <- Year_2011 |> 
  add_row(Year_2012) |> 
  add_row(Year_2013) |> 
  add_row(Year_2014) |> 
  add_row(Year_2015) |> 
  add_row(Year_2016) |> 
  add_row(Year_2017) |> 
  add_row(Year_2018) |> 
  add_row(Year_2019) |> 
  add_row(Year_2020) |> 
  add_row(Year_2021) |> 
  add_row(Year_2022) |> 
  add_row(Year_2023)

write_csv(x = Combined_Years, file = "BLS_Combined_Years.csv")

Combined_Years_backup <- Combined_Years
# Combined_Years <- Combined_Years_backup

Combined_Years <- Combined_Years |> 
  # Gather the columns month1_emplvl, month2_emplvl, and month3_emplvl into key-value pairs
  gather(key = "month", value = "emplvl",  month1_emplvl, month2_emplvl, month3_emplvl) |> 
  # Extract the numeric part from the "month" column
  mutate(
    month =
      case_when(
        month == "month1_emplvl" ~ 1,
        month == "month2_emplvl" ~ 2,
        month == "month3_emplvl" ~ 3
      )
  )

Combined_Years <- Combined_Years |> arrange(area_title, year, qtr)


# Change the file location below to where you want to save the csv

pivot_combined <- Combined_Years |>  pivot_wider(
  id_cols = c("area_fips", "area_title"),
  names_from = c("year", "qtr", "month"),
  values_from = c(
    # "qtrly_estabs_count", 
    "emplvl",
  ),
  names_sep = ""
)

pivot_combined$`201111_pct_change` <- 0

# Calculate percent change for each pair of columns and create new columns for the results
for (i in 3:(ncol(pivot_combined) - 2)) {
  # Calculate percent change using the formula: ((new value - old value) / old value) * 100
  col_name <- colnames(pivot_combined)[i + 1]
  new_col_name <- paste0(col_name, "_pct_change")
  
  pivot_combined[[new_col_name]] <- ((pivot_combined[[i + 1]] - pivot_combined[[3]]) / pivot_combined[[3]]) * 100
}

# The new columns with percent change values have been added to the dataframe

pivot_sub <- pivot_combined[,c(2,150:296)]

transpose <- pivot_sub |> t() |> data.frame()
colnames(transpose) <- transpose[1,]
transpose <- transpose[-1,]
transpose <- cbind(
  Year = str_split(string = rownames(transpose), pattern = "_", simplify = T)[,1] |> as.numeric(),
  transpose
)
rownames(transpose) <- NULL

transpose_plot <- transpose |> gather(key = "County", value = "Employment Change", -Year)



transpose_plot$old_year <- transpose_plot$Year

transpose_plot <- transpose_plot |>
  mutate(
    Year =
      substr(transpose_plot$old_year,start = 1,stop = 4) |> 
      as.numeric() + 
      (substr(old_year, start = 5, stop = 5) |> 
         as.numeric() - 1) * 0.25 +
      (substr(old_year, start = 6, stop = 6) |> 
         as.numeric() - 1) * (1/12)
  )

# Filter out values < -50
transpose_plot <- transpose_plot |> 
  filter(`Employment Change` > -90)


MO_sub <- transpose_plot[str_detect(string = transpose_plot$County, pattern = "Missouri"),]

MO_Before_2018 <- MO_sub |> filter(Year < 2018)

MO_After_2018 <- MO_sub |> filter(Year > 2018)

lm_missouri_before <- lm(
  data = MO_Before_2018,
  formula = `Employment Change` ~ Year
)

lm_missouri_after <- lm(
  data = MO_After_2018,
  formula = `Employment Change` ~ Year
)

lm_missouri_overall <- lm(
  data = MO_sub,
  formula = `Employment Change` ~ Year
)

plot(
  x = MO_sub$Year,
  y = MO_sub$`Employment Change`,
  xlab = "Year",
  ylab = "Employment level change since 2011 (%)",
  ylim = c(-60, 80),
  las = 1
)

abline(lm_missouri_before, col = 'red', lwd = 2) # Before Min Wage Increase
abline(lm_missouri_after, col = 'green', lwd = 2) # After
abline(v = 2018, col = 'blue', lwd = 2) # Min wage Increase

legend(
  "bottomleft", # Position of the legend on the plot
  legend = c("Regression Line (Before)", "Regression Line (After)", "Min Wage Increase"),
  col = c("red", "green", "blue"), # Colors corresponding to the lines
  lwd = 2, # Line width
  # pch = 16, # Point type
  title = "Legend" # Legend title
)


abline(lm_missouri_overall, col = 'blue', lwd = 2)

# plotting before 2018 only
plot(
  x = MO_Before_2018$Year,
  y = MO_Before_2018$`Employment Change`
)

ggplot(transpose_plot) +
  aes(
    x = Year
  )
>>>>>>> 44b51e74884c54d4da212494cbe7957767859c3a
