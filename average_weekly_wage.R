# Code from Matthew Marsh

wrangled$limited_combined$combined$state <- str_detect(wrangled$limited_combined$combined$area_title, pattern = "Missouri") |> 
  ifelse(test = _,yes = "MO",no =  "KS")

change_avg_wkly_wg <- wrangled$limited_combined$combined |>
  aggregate(avg_wkly_wage ~ state + year, FUN = mean)# |>
  pivot_wider(
    names_from = year,
    values_from = avg_wkly_wage)

change_avg_wkly_wg

ggplot(
  data = change_avg_wkly_wg,
  aes(
    x = year,
    y = avg_wkly_wage,
    fill = state)) + 
  geom_bar(
  stat = "identity", 
  position = position_dodge(width = 0.85), 
  color = 'black', 
  # linewidth = 0.01,
  width = 0.75) + 
  scale_x_continuous(breaks = 2011:2023) +
  labs(
    title = "Average Weekly Wage",
    x = "Year",
    y = "US Dollars"
  ) + 
  guides(fill = guide_legend(title = "State")) +
  theme_light() +
  scale_fill_manual(values = c("MO" = "red2", "KS" = "blue2"))

ggsave(
  filename = "average_weekly_wage.png",
  dpi = 'retina',
  width = 10.4,
  height = 4.81)
  

