# Run regressions script to create necessary data frames
source('regressions.R')

############ #
# GGplots ####
############ #
# 
# ignore this function
select_years <- function(start_month, start_yr, end_month, end_yr) {
  start <- (start_month - 1) / 12 + start_yr
  end <- (end_month - 1) / 12 + end_yr
  return(seq(start, end, by = 1/12))
}

# select which years you are interested in:
deci_period_means_trend <- select_years(
  start_month = 1,
  start_yr = 2011,
  end_month = 3,
  end_yr = 2023
)

# use this for all months:unique(joined$data$year_decimal)

# Set loess span
loess_span <- 0.3

ggplot(data = joined$data |> 
         filter(year_decimal %in% deci_period_means_trend), 
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
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
stat_summary(
  fun = mean,
  geom = "point",
  size = 2) +
scale_x_continuous(
breaks = deci_period_means_trend,
limits = range(deci_period_means_trend)
)

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

ggsave(
  plot = plot_name_here,
  filename = "means_plot_11_12_no_loess.png", 
  dpi = 'retina',
  width = 10.4,
  height = 4.81)



############################################ #
# Not using stuff below for now (11/29)
############################################ #
deci_period_prop_chg <- unique(joined$data$year_decimal)# seq(2011.5, 2022.5, by = 1)

# joined$data |> 
#   filter(year_decimal %in% deci_period_prop_chg & state == "KS") |> 
#   aggregate(proportion_limited ~ state + year_decimal, FUN = mean) |> 
#   arrange(state) |> 
#   mutate(change = c(NA, diff(proportion_limited))) |> 
#   add_row(
#     joined$data |> 
#       filter(year_decimal %in% deci_period_prop_chg & state == "MO") |> 
#       aggregate(proportion_limited ~ state + year_decimal, FUN = mean) |> 
#       arrange(state) |> 
#       mutate(change = c(NA, diff(proportion_limited)))) |> 
#   pivot_wider(
#     names_from = state,
#     values_from = c('proportion_limited', 'change')) #|> 
# write_csv(file = "prop_chg.csv")
# select(-proportion_limited) |> 
# pivot_wider(
#   names_from = state,
#   values_from = change
# )
# ggsave(filename = "means_with_loess.png", dpi = 'retina')
# Calculate means ####


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

# Proportion working in limited service
# Most recent plot made by Matt Marsh

ggplot(data = joined$data |> filter(year_decimal %in% deci_period_means_trend), 
       aes(x = year_decimal, y = proportion_limited, color = State)) +
  # geom_point(position = position_dodge(width = 0.02)) +
  # geom_smooth(method = "loess", se = FALSE, span = 0.1) +
  geom_line(
    aes(group = State),
    stat = "summary",
    fun = mean,
    lwd = 1) +
  labs(title = "Means Plot, 2011 - 2023", x = "Year", y = "Proportion Employed in Limited Service Restaurants") +
  geom_vline(aes(xintercept = 2013, linetype = "Small"),
             lwd = .6,
             linetype = "dashed") +
  geom_vline(aes(xintercept = 2014, linetype = "Small"),
             lwd = .6,
             linetype = "dashed") +
  geom_vline(aes(xintercept = 2015, linetype = "Small"),
             lwd = .6,
             linetype = "dashed") +
  geom_vline(aes(xintercept = 2017, linetype = "Small"),
             lwd = .6,
             linetype = "dashed") +
  geom_vline(aes(xintercept = 2018, linetype = "Small"),
             lwd = .6,
             linetype = "dashed") +
  geom_vline(aes(xintercept = 2019, linetype = "Large"),
             lwd = .6) +
  geom_vline(aes(xintercept = 2020, linetype = "Large"),
             lwd = .6) +
  geom_vline(aes(xintercept = 2021, linetype = "Large"),
             lwd = .6) +
  geom_vline(aes(xintercept = 2022, linetype = "Large"),
             lwd = .6) +
  scale_color_manual(name = "Minimum Wage Increase", values = c("Small" = "#2874AE", "Large" = "#2874AE", "MO" = "#2874AE", "KS" = "brown1"), guide = guide_legend(override.aes = list(linetype = c("dashed", "solid")))) +
  scale_x_continuous(breaks = seq(2011, 2023, by = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust = 1))