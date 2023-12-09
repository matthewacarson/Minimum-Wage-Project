library(tidyverse)
# Load data ####

hypothetical_data <- read_csv("C:\\Users\\madou\\OneDrive - UCLA IT Services\\2)_2023_Fall\\PS-170A\\Minimum-Wage-Project\\hypothetical-data.csv")

# sapply(hypothetical_data$Date, function(x)strsplit(x, "/")[[1]])[3,] |> as.numeric()

hypothetical_data$years <- hypothetical_data$Date |> as.Date(format = "%m/%d/%Y") |> year()

# GGplot change plots ####
ggplot(
  data = hypothetical_data |> 
    filter(years >= 2010)
) + 
  aes(x = years, y = MW_Change_Abs, fill = Region) +
  geom_bar(stat = 'identity', position = 'stack') + # 'dodge') +
  scale_x_continuous(breaks = 1991:2023) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ylim(c(0,2)) +
  labs(
    y = "Minimum wage change from pervious year (absolute $)",
    x = "Year"
  )

# AB Line ####
# 

sub_2011_2017 <- hypothetical_data |>
  filter(years >= 2011 & years <= 2017 & Region == "MO")

lm_1 <- lm(
  data = sub_2011_2017,
  Unemp_Hypo ~ years
)

plot(
  x = sub_2011_2017$years,
  y = sub_2011_2017$Unemp_Hypo
)

abline(lm_1)

abline(h = mean_2011_2017)

# Base R change plots ####
# 

plot(
  x = hypothetical_data$years[1:33],
  y = hypothetical_data$MW_Change[hypothetical_data$Region == "MO"],
  cex = 1,
  col = 'red',
  las = 1,
  xaxt = 'n',
  ylab = "Minimum wage change relative to previous year (proportion)",
  pch = 16
)

axis(side = 1, at = hypothetical_data$years[1:33], las = 2)


lines(
  x = hypothetical_data$years[1:33],
  y = hypothetical_data$MW_Change[hypothetical_data$Region == "Fed/KS"],
  col = 'blue',
  lwd = 2
)

lines(
  x = hypothetical_data$years[1:33],
  y = hypothetical_data$MW_Change[hypothetical_data$Region == "MO"],
  col = 'red',
  lwd = 2
)
points(
  x = hypothetical_data$years[1:33],
  y = hypothetical_data$MW_Change[hypothetical_data$Region == "Fed/KS"],
  col = 'blue',
  cex = 1,
  pch = 16
)

legend(
  "topleft", 
  legend = c("MO", "KS"), 
  col = c('red', 'blue'), 
  lty = 1,
  lwd = 3
)


# Abs plot ####
# 
plot(
  x = hypothetical_data$years[1:33],
  y = hypothetical_data$MW_Change_Abs[hypothetical_data$Region == "MO"],
  cex = 1,
  col = 'red',
  las = 1,
  xaxt = 'n',
  ylab = "Minimum wage change from previous year (absolute $)",
  pch = 16
)

axis(side = 1, at = hypothetical_data$years[1:33], las = 2)


lines(
  x = hypothetical_data$years[1:33],
  y = hypothetical_data$MW_Change_Abs[hypothetical_data$Region == "Fed/KS"],
  col = 'blue',
  lwd = 2
)

lines(
  x = hypothetical_data$years[1:33],
  y = hypothetical_data$MW_Change_Abs[hypothetical_data$Region == "MO"],
  col = 'red',
  lwd = 2
)
points(
  x = hypothetical_data$years[1:33],
  y = hypothetical_data$MW_Change_Abs[hypothetical_data$Region == "Fed/KS"],
  col = 'blue',
  cex = 1,
  pch = 16
)

legend(
  "topleft", 
  legend = c("MO", "KS"), 
  col = c('red', 'blue'), 
  lty = 1,
  lwd = 3
)


# Absolute Numbers ####
# 

plot(
  x = hypothetical_data$years[1:33],
  y = hypothetical_data$Min_Wage[hypothetical_data$Region == "MO"],
  cex = 1,
  col = 'red',
  xaxt = 'n',
  yaxt = 'n',
  las = 1,
  ylab = "Minimum Wage",
  xlab = "Year",
  pch = 16
)

axis(side = 1, at = hypothetical_data$years[1:33], las = 2)
axis(side = 2, at = 4:12, las = 1)

lines(
  x = hypothetical_data$years[1:33],
  y = hypothetical_data$Min_Wage[hypothetical_data$Region == "Fed/KS"],
  col = 'blue',
  lwd = 2
)

lines(
  x = hypothetical_data$years[1:33],
  y = hypothetical_data$Min_Wage[hypothetical_data$Region == "MO"],
  col = 'red',
  lwd = 2
)

points(
  x = hypothetical_data$years[1:33],
  y = hypothetical_data$Min_Wage[hypothetical_data$Region == "Fed/KS"],
  col = 'blue',
  cex = 1,
  pch = 16
)

legend(
  "topleft", 
  legend = c("MO", "KS"), 
  col = c('red', 'blue'), 
  lty = 1,
  lwd = 3
)

# Load BLS data ####
# 


# Testing search function ####

str_detect(c("Johnson, Missouri", "Jackson, Kansas", "Wynnoite, Kansas"), pattern = "Kansas")


add_row()


# Census Data ####
# Probably not going to use this!!

FastFood <- get_acs(
  geography = 'county',
  # variables = 'S2405',
  year = 2010,
  county = 'Johnson',
  state = 'KS',
  survey = 'acs1',
  table = 'S2405',
  # output = 'wide'
)

# https://data.census.gov/table/ACSST1Y2010.S2405?q=Johnson+County,+Kansas+Employment&t=Industry

v10 <- load_variables(2010, "acs5", cache = TRUE)


v10[str_detect(v10$label,pattern = "fast"),]
