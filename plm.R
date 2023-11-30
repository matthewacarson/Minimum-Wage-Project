
# Run panel data model with area_title as a fixed effect
panel_model <-
  plm::plm(emplvl_limited ~ treat * post + emplvl_all + area_title,
      model = "within",
      index = "area_title",
      effect = "twoways",
      data = data_2012_2013)

summary(panel_model)

panel_model <-
  plm::plm(emplvl_limited ~ min_wage + emplvl_all + area_title + date,
           model = "within",
           index = c("area_title", "date"),
           effect = "twoways",
           data = joined$data)

summary(panel_model)


# Install and load the required package
# install.packages("plm")
library(plm)

# Assuming your data is in a data.frame called 'joined' with columns 'emplvl_limited', 'min_wage', 'emplvl_all', 'area_title', and 'date'

# Convert 'date' to a Date class if it's not already
# joined$date <- as.Date(joined$date)

# Create a panel data object
pdata <- pdata.frame(joined$data, index = c("area_title", "date"))

# Run the fixed-effects model
model_fe <- 
  pdata |> 
  plm(
    emplvl_limited ~ min_wage + emplvl_all, 
    data = _, 
    model = "within")

# Print the results
summary(model_fe)
