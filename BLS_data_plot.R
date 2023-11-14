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
  "month3_emplvl", "industry_code"
)

Year_2011_f <- Year_2011[
  str_detect(
    string = Year_2011$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Year_2012_f <- Year_2012[
  str_detect(
    string = Year_2012$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Year_2013_f <- Year_2013[
  str_detect(
    string = Year_2013$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Year_2014_f <- Year_2014[
  str_detect(
    string = Year_2014$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Year_2015_f <- Year_2015[
  str_detect(
    string = Year_2015$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Year_2016_f <- Year_2016[
  str_detect(
    string = Year_2016$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Year_2017_f <- Year_2017[
  str_detect(
    string = Year_2017$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]


Year_2018_f <- Year_2018[
  str_detect(
    string = Year_2018$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Year_2019_f <- Year_2019[
  str_detect(
    string = Year_2019$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Year_2020_f <- Year_2020[
  str_detect(
    string = Year_2020$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Year_2021_f <- Year_2021[
  str_detect(
    string = Year_2021$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Year_2022_f <- Year_2022[
  str_detect(
    string = Year_2022$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]

Year_2023_f <- Year_2023[
  str_detect(
    string = Year_2023$area_title,
    pattern = counties_of_interest
  ), filterd_columns
]



PS_170_Minimum_Wage_Master_Data_Set <- 
  read_csv("BLS/PS 170 Minimum Wage Master Data Set.csv")

Combined_Years <- 
  Year_2011_f |> 
  add_row(Year_2012_f) |> 
  add_row(Year_2013_f) |> 
  add_row(Year_2014_f) |> 
  add_row(Year_2015_f) |> 
  add_row(Year_2016_f) |> 
  add_row(Year_2017_f) |> 
  add_row(Year_2018_f) |> 
  add_row(Year_2019_f) |> 
  add_row(Year_2020_f) |> 
  add_row(Year_2021_f) |> 
  add_row(Year_2022_f) |> 
  add_row(Year_2023_f)

Combined_Years$area_fips <- as.numeric(Combined_Years$area_fips)

Combined_Years_All_Ind <- 
  add_row(PS_170_Minimum_Wage_Master_Data_Set)

# Matt (11/13): Stopped here in adding the other data set

Combined_Years_gather <- Combined_Years |> 
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

Combined_Years_arrange <- Combined_Years |> select(
  area_fips, area_title, year, qtr, month, qtrly_estabs_count, emplvl
) |> arrange(area_title, year, qtr)


# Change the file location below to where you want to save the csv

Combined_Years_pivot <- Combined_Years |>  pivot_wider(
  id_cols = c("area_fips", "area_title"),
  names_from = c("year", "qtr", "month"),
  values_from = c(
    # "qtrly_estabs_count", 
    "emplvl",
  ),
  names_sep = ""
)



Combined_Years_pivot$`201111_pct_change` <- 0

# Calculate percent change for each pair of columns and create new columns for the results
for (i in 3:(ncol(Combined_Years_pivot) - 2)) {
  # Calculate percent change using the formula: ((new value - old value) / old value) * 100
  col_name <- colnames(Combined_Years_pivot)[i + 1]
  new_col_name <- paste0(col_name, "_pct_change")
  
  Combined_Years_pivot[[new_col_name]] <- ((Combined_Years_pivot[[i + 1]] - Combined_Years_pivot[[3]]) / Combined_Years_pivot[[3]]) * 100
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

transpose_plot <- transpose |> gather(key = "County", value = "Employment_Change", -Year)



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
# transpose_plot <- transpose_plot |> 
  # filter(Employment_Change > -90)

transpose_plot$Employment_Change <- as.numeric(transpose_plot$Employment_Change)
#################################### #
# Begin Analysis ####
#################################### #
## Missouri ####
#################################### #
treatment_year <- 2019
# Subset for MO
# 


MO_sub <- transpose_plot[str_detect(string = transpose_plot$County, pattern = "Missouri"),]

MO_Before_2019 <- MO_sub |> filter(Year < treatment_year)


MO_After_2019 <- MO_sub |> filter(Year >= treatment_year)


lm_missouri_before <- lm(
  data = MO_Before_2019,
  formula = Employment_Change ~ Year
)

lm_missouri_after <- lm(
  data = MO_After_2019,
  formula = Employment_Change ~ Year
)

lm_missouri_overall <- lm(
  data = MO_sub,
  formula = Employment_Change ~ Year
)

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

lm_kansas_before <- lm(
  data = KS_Before_2019,
  formula = Employment_Change ~ Year
)

lm_kansas_after <- lm(
  data = KS_After_2019,
  formula = Employment_Change ~ Year
)

lm_kansas_overall <- lm(
  data = KS_sub,
  formula = Employment_Change ~ Year
)

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

ggplot(data = KS_sub, aes(x = Year, y = Employment_Change)) +
  # Add a line for Employment Change
  geom_point(color = 'black') +
  geom_smooth(method = "lm", formula = y ~ x, data = KS_Before_2019, color = "blue", se = TRUE) +
  geom_smooth(method = 'lm', formula = y ~ x, data = KS_After_2019, color = "red", se = TRUE) +
  labs(
    title = "KS"
  ) + geom_vline(xintercept = 2019)
