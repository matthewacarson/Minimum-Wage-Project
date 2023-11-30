# Run data wrangling file first ####
source(file = "limited_serv_wrangling.R", local = wrangled <- new.env())
source(file = "all_industries_data_wrangling.R", local = wrangled)

# Joining all industries and limited service
# library(fastDummies)

joined <- new.env()
joined$data <- full_join(
  x = wrangled$min_wage$limited_increase,
  y = wrangled$all_ind_combined$gather) |> 
  mutate(
    # year_2018 = ifelse(year(date) == 2018, 1, 0),
    proportion_limited = emplvl_limited / emplvl_all) |>
  # rename(State = state) |>
  # arrange(date) |> 
  na.omit() |> 
  # filter(proportion_limited > 0 & proportion_limited < Inf) |> 
  mutate(
    area_fips = factor(area_fips)
    # year = as.character(year_numeric)
    # Treatment_Group = State == "MO",
    # mw_increase = min_wage - min(min_wage)
  )
  # dummy_columns( # dummy columns as 'treatment' variable in regression
    # select_columns = 'year',
    # remove_first_dummy = TRUE)# c("State", "year"))

# 
# summary(panel_model)
library(fastDummies)
data_2012_2013 <- joined$data |> 
  filter(year %in% 2012:2013) |> 
  dummy_columns(
    select_columns = c('year', 'state'), remove_first_dummy = TRUE, 
  ) |> 
  rename(post = year_2013, treat = state_MO)
  

# #################### #
# #######D-i-D #######
# #################### #
models <- new.env()
library(lfe)

felm_all <-
  felm(proportion_limited ~ treat * post | area_title,
       data = data_2012_2013)

summary(felm_all)

felm_all <-
  felm(emplvl_limited ~ treat * post + emplvl_all | area_title,
       data = data_2012_2013)

summary(felm_all)



models$felm_2012_2013 <- joined$data |> 
  filter(year %in% 2012:2013) |> mutate(year = as.factor(year)) |> 
  felm(emplvl_limited ~ State * year + emplvl_all | area_title, 
       data = _)

summary(models$felm_2012_2013)

# try using this with felm:  xactDOF = TRUE)

############ #
# GGplots ####
############ #
deci_period_prop_chg <- seq(2011.5, 2022.5, by = 1)

joined$data |> 
  filter(year_decimal %in% deci_period_prop_chg & State == "KS") |> 
  aggregate(proportion_limited ~ State + year_decimal, FUN = mean) |> 
  arrange(State) |> 
  mutate(change = c(NA, diff(proportion_limited))) |> 
  add_row(
    joined$data |> 
      filter(year_decimal %in% deci_period_prop_chg & State == "MO") |> 
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

# Checking parallel trends assumption

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