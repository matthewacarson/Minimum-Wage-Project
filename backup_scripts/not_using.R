
# ################################################################### #
# ################################################################### #
# ################################################################### #
# All code below needs to be modified because of changes made above # #
# ################################################################### #
# ################################################################### #
# ################################################################### #

## Begin Regressions ####
# Regression from Eve

# lm_1 <- felm(
#   Employment ~ State + Year + Min_Wage + State:Year | as.factor(County),
#   data = transpose_gather)
# 
# lm_missouri_before <- lm(
#   data = transpose_gather |>
#     filter(State == "Missouri" & Year < 2019),
#   formula = Employment ~ Year)
# 
# lm_missouri_after <- lm(
#   data = transpose_gather |>
#     filter(State == "Missouri" & Year >= 2019),
#   formula = Employment ~ Year)
# 
# lm_missouri_overall <- lm(
#   data = transpose_gather |>
#     filter(State == "Missouri"),
#   formula = Employment ~ Year)
# 
# lm_kansas_before <- lm(
#   data = transpose_gather |>
#     filter(State == "Kansas" & Year < 2019),
#   formula = Employment ~ Year)
# 
# lm_kansas_after <- lm(
#   data = transpose_gather |>
#     filter(State == "Kansas" & Year >= 2019),
#   formula = Employment ~ Year)
# 
# lm_kansas_overall <- lm(
#   data = transpose_gather |>
#     filter(State == "Kansas"),
#   formula = Employment ~ Year)
# 
# summary(lm_missouri_before)
# summary(lm_kansas_before)
# 
# # Difference in slopes
# lm_missouri_before$coefficients[2] - lm_kansas_before$coefficients[2]
# 
# # Trying out the lm model from office hour (11/15)
# 
# lm_full <- lm(
#   formula = Employment ~
#     Year +
#     Post2019 +
#     State +
#     State:Post2019 +
#     factor(County),
#   data = transpose_gather)
# 
# summary(lm_full)

# library(sandwich)
# library(lmtest)
#
# # Perform a test for the difference in slopes
# # (I don't think this is the right test.)
# coeftest(lm_full, vcov = vcovHC(lm_full))
#
# library(lsmeans)
#
# lstrends(lm_missouri_before, lm_kansas_before, var = "Year")
# confint(lm_missouri_before, parm = "Year", level = 0.9)
# confint(lm_kansas_overall, parm = "Year", level = 0.9)

#################################### #
# Begin Analysis ####
#################################### #
# coefficients_lm_missouri_before <-
#   coef(lm_missouri_before)
# # main_plot <-
# ggplot(data = transpose_gather,
#        aes(x = Year, y = Employment, col = State)) +
#   geom_point() +
#   geom_vline(aes(xintercept = 2019, linetype = "$0.8 Increase"),
#              col = 'green4', lwd = 0.9) +
#   geom_abline(
#     aes(slope = coefficients_lm_missouri_before[2],
#         intercept = coefficients_lm_missouri_before[1],
#         color = "Missouri"),
#     lwd = 0.9,
#     show.legend = F
#     ) +
#   geom_smooth(
#     method = "lm",
#     formula = y ~ x,
#     data = transpose_gather |>
#       filter(State == "Missouri" & Year >= 2019),
#     color = 'black',
#     linetype = "dashed",
#     se = F,
#     lwd = 0.9
#     # aes(color = "MO After")
#   ) +
#   geom_smooth(
#     method = "lm",
#     formula = y ~ x,
#     data = transpose_gather |>
#       filter(State == "Kansas"),
#     # color = 'red3',
#     linetype = "solid",
#     se = F,
#     lwd = 0.9,
#     # aes(color = "KS")
#   ) +
#   labs(
#     title =
#       "Limited-Service Food Industry Employment",
#     subtitle =
#       "Proportion of total employed working in limited-service restaurants.",
#     x =
#       "Year",
#     y =
#       "Proportion working in limited-service restaurants",
#     fill =
#       "State",
#     linetype = "") +
#   # scale_color_manual(values = c("Missouri" = "black", "Kansas" = "red3", "Vertical Line" = "green4")) +
#   scale_linetype_manual(values = c("$0.8 Increase" = "solid")) +
#   theme_light()
# 
# ggsave(
#   filename = "Rplot08.png",
#   plot = main_plot,
#   height = 800 * 3,
#   width = 1400 * 3,
#   units = 'px'
# )
# geom_smooth(
#   method = "lm",
#   formula = y ~ x,
#   data = transpose_gather |>
#     filter(State == "Missouri" & Year < 2019),
#   # color = 'blue4',
#   linetype = "solid",
#   se = F,
#   # aes(color = "MO Overall")
# ) +





# ##################################################################### #
# ##################################################################### #
# ################ NOT USING CODE BELOW AS OF 11/14 #####################
# ##################################################################### #
# ##################################################################### #

# ggplot(data = transpose_gather,
#        aes(x = Year, y = Employment, col = State)) +
#   geom_point() +
#   geom_vline(xintercept = 2019, col = 'green3', lwd = 0.9) +
#   geom_smooth(
#     method = "lm",
#     formula = y ~ splines::ns(x, df = 10),
#     # formula = y ~ x,
#     data = transpose_gather |>
#       filter(State == "Missouri"),
#     color = 'blue2',
#     se = F
#   ) +
#   geom_smooth(
#     method = "lm",
#     formula = y ~ splines::ns(x, df = 10),
#     # formula = y ~ x,
#     data = transpose_gather |>
#       filter(State == "Kansas"),
#     color = 'red3',
#     se = F
#   ) #+
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
# filter(Employment > -90)

#################################### #
# treatment_year <- 2019
# Subset for MO

# MO_sub <- transpose_plot[str_detect(string = transpose_plot$County, pattern = "Missouri"),]
# MO_Before_2019 <- MO_sub |> filter(Year < treatment_year)
# MO_After_2019 <- MO_sub |> filter(Year >= treatment_year)
# 
# plot(
#   x = MO_sub$Year,
#   y = MO_sub$Employment,
#   xlab = "Year",
#   ylab = "Employment level change since 2011 (%)",
#   ylim = c(-70, 80),
#   las = 1,
#   main = "Missouri"
# )
# 
# abline(lm_missouri_before, col = 'red', lwd = 2) # Before Min Wage Increase
# abline(lm_missouri_after, col = 'blue', lwd = 2) # After
# #
# # Creating manually
# line_difference <- predict(lm_missouri_before, newdata = data.frame(Year = treatment_year)) - predict(lm_missouri_after, newdata = data.frame(Year = treatment_year))

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

# abline(
#   a = coef(lm_missouri_after)[1] + line_difference,
#   b = coef(lm_missouri_after)[2],
#   col = 'blue',
#   lwd = 2
# )
# 
# abline(v = treatment_year, col = 'orange', lwd = 2) # Min wage Increase
# 
# legend(
#   "bottomleft", # Position of the legend on the plot
#   legend = c("Regression Line (Before)", "Regression Line (After)", "Min Wage Increase"),
#   col = c("red", "blue", "orange"), # Colors corresponding to the lines
#   lwd = 2, # Line width
#   # pch = 16, # Point type
#   title = "Legend" # Legend title
# )
# 
# 
# abline(lm_missouri_overall, col = 'green', lwd = 2)

# Plotting after 2018 only

# ggplot(data = MO_sub, aes(x = Year, y = Employment)) +
#   # Add a line for Employment Change
#   geom_point(color = 'black') +
#   geom_smooth(method = "lm", formula = y ~ x, data = MO_Before_2019, color = "blue", se = TRUE) +
#   geom_smooth(method = "lm", formula = y ~ x, data = MO_After_2019, color = "red", se = TRUE) +
#   labs(
#     title = "MO"
#   ) + geom_vline(xintercept = 2019)
# 
# ############################## #
# ############################## #
# 
# KS_sub <- transpose_plot[str_detect(string = transpose_plot$County, pattern = "Kansas"),]
# KS_sub$Employment[is.infinite(KS_sub$Employment)] <- 100
# 
# KS_Before_2019 <- KS_sub[KS_sub$Year < treatment_year,]
# 
# KS_After_2019 <- KS_sub[KS_sub$Year >= treatment_year,]
# 
# 
# # Edit everything below before running so that it is for KS instrad of MO
# #
# 
# 
# plot(
#   x = KS_sub$Year,
#   y = KS_sub$Employment,
#   xlab = "Year",
#   ylab = "Employment level change since 2011 (%)",
#   # ylim = c(-70, 80),
#   las = 1,
#   main = "Kansas"
# )
# 
# abline(lm_kansas_before, col = 'red', lwd = 2) # Before Min Wage Increase
# abline(lm_kansas_after, col = 'green', lwd = 2) # After
# #
# # Creating manually
# line_difference <- predict(lm_kansas_before, newdata = data.frame(Year = treatment_year)) - predict(lm_kansas_after, newdata = data.frame(Year = treatment_year))

################## #
# Segments aren't plotting right
################## #

# segments(
#   x0 = treatment_year,
#   y0 = predict(lm_kansas_before, newdata = data.frame(Year = treatment_year)),
#   x1 = 2025,
#   y1 = predict(lm_kansas_after, newdata = data.frame(Year = max(KS_After_2019$Year))) + line_difference,
#   col = 'blue',
#   lwd = 2,
#   lty = 2
# )

# abline(
#   a = coef(lm_kansas_after)[1] + line_difference,
#   b = coef(lm_kansas_after)[2],
#   col = 'blue',
#   lwd = 2
# )
# 
# abline(v = treatment_year, col = 'orange', lwd = 2) # Min wage Increase
# 
# legend(
#   "bottomleft", # Position of the legend on the plot
#   legend = c("Regression Line (Before)", "Regression Line (After)", "Min Wage Increase"),
#   col = c("red", "blue", "orange"), # Colors corresponding to the lines
#   lwd = 2, # Line width
#   # pch = 16, # Point type
#   title = "Legend" # Legend title
# )
# 
