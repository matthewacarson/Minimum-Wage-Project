# Run data wrangling files first ####
source(file = "limited_serv_wrangling.R", local = wrangled <- new.env())
source(file = "all_industries_data_wrangling.R", local = wrangled)

# Joining all industries and limited service ####

joined <- new.env()
joined$data <- full_join(
  x = wrangled$min_wage$limited_increase,
  y = wrangled$all_ind_combined$gather) |> 
  mutate(
    proportion_limited = emplvl_limited / emplvl_all) |>
  filter(proportion_limited > 0 & !is.infinite(proportion_limited)) |> 
  na.omit() |>
  mutate(
    area_fips = factor(area_fips)
  )

# Checking if there are any zeros
range(joined$data$proportion_limited)
range(joined$data$emplvl_all)
range(joined$data$emplvl_limited)
any(joined$data[,1:21] == 0)
# Checking how many times each county appears
table(joined$data$year, joined$data$area_title)


# write_csv(x = joined$data, file = "joined_data.csv")

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

# Checking if there are any zeros
range(data_2012_2013$proportion_limited)
range(data_2012_2013$emplvl_all)
range(data_2012_2013$emplvl_limited)
any(data_2012_2013[,1:21] == 0)
# Checking how many times each county appears
table(data_2012_2013$year, data_2012_2013$area_title)

# #################### #
# #######D-i-D #######
# #################### #
# models <- new.env()
library(lfe)

felm_proportion <-
  felm(proportion_limited ~ treat * post | area_title,
       data = data_2012_2013)

summary(felm_proportion)

felm_all <-
  felm(emplvl_limited ~ treat * post + emplvl_all | area_title,
       data = data_2012_2013)

summary(felm_all)

felm_cont_treatment <- 
  felm(emplvl_limited ~ min_wage + emplvl_all | area_title + date, 
       data = joined$data)

summary(felm_cont_treatment)


# try using this with felm:  xactDOF = TRUE)

############ #
# GGplots ####
############ #
deci_period_prop_chg <- seq(2011.5, 2022.5, by = 1)

joined$data |> 
  filter(year_decimal %in% deci_period_prop_chg & state == "KS") |> 
  aggregate(proportion_limited ~ state + year_decimal, FUN = mean) |> 
  arrange(state) |> 
  mutate(change = c(NA, diff(proportion_limited))) |> 
  add_row(
    joined$data |> 
      filter(year_decimal %in% deci_period_prop_chg & state == "MO") |> 
      aggregate(proportion_limited ~ state + year_decimal, FUN = mean) |> 
      arrange(state) |> 
      mutate(change = c(NA, diff(proportion_limited)))) |> 
  pivot_wider(
    names_from = state,
    values_from = c('proportion_limited', 'change')) #|> 
# write_csv(file = "prop_chg.csv")
# select(-proportion_limited) |> 
# pivot_wider(
#   names_from = state,
#   values_from = change
# )
# ggsave(filename = "means_with_loess.png", dpi = 'retina')
# Calculate means ####
# 
# select which years youa re interested in:
deci_period_means_trend <- unique(joined$data$year_decimal)# seq(unique(joined$data$year_decimal), by = 1/12)

# Checking parallel trends assumption

means_per_period <- joined$data |> 
  filter(year_decimal %in% deci_period_means_trend) |> 
  aggregate(proportion_limited ~ year_decimal + state, FUN = mean) |> 
  pivot_wider(
    names_from = state,
    values_from = proportion_limited
  ) |> data.frame()
diff_KS <- diff(means_per_period$KS)
diff_MO <- diff(means_per_period$MO)

lm_diffs <- lm(diff_MO ~ diff_KS)
plot(
  x = diff_KS,
  y = diff_MO,
  col = 'dodgerblue', 
  pch = 16
)
abline(lm_diffs, col = 'red')
abline(b = 1, a = 0, col = 'blue')

# plot(lm_diffs)

# write_csv(x = means_per_period, file = "means_per_period.csv")

cor(means_per_period$KS, means_per_period$MO)

# scatter plot ####

# Set loess span
loess_span <- 0.3

ggplot(data = joined$data |> filter(year_decimal %in% deci_period_means_trend), 
       aes(x = year_decimal, y = proportion_limited, color = state)) +
  # geom_point(position = position_dodge(width = 0.02)) +
  geom_smooth(method = "loess", se = FALSE, span = loess_span) +
  geom_line(
    aes(group = state),
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
       aes(x = year_decimal, y = proportion_limited, color = state)) +
  geom_smooth(method = "loess", se = FALSE, span = loess_span, lwd = 0.75) +
  stat_summary(
    fun = mean,
    size = 2,
    geom = "point") +
  geom_line(
    aes(group = state),
    stat = "summary",
    lwd = 0.7,
    fun = mean) +
  # scale_x_continuous(
  # breaks = seq(2016, 2020, by = 0.25),
  # limits = c(2011, 2023.25)
  # ) +
  labs(
    title = "Mean Per Month",
    subtitle = "Mean proportion within each county of total employed in limited service restaurants",
    caption = "*Loess line with span of 0.3",
    x = "Year",
    y = "Mean Proportion",
    color = "State*"
    ) +
  scale_y_continuous(breaks = seq(0.025, 0.05, by = 0.0025)) +
  theme_minimal() +
  theme(
    legend.position = "right",
    # plot.caption = element_text(
      # hjust = 1.15, vjust = 0.7),# margin = margin(t = 10, b = 10)),
    axis.text.x = element_text(
      angle = 90, vjust = 0.5, hjust = 1))

# ggsave(filename = "means_plot_11_12_no_loess.png", dpi = 'retina')
