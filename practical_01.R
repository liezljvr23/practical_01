# Task 1: set up R project and initialise Github repository

# Install and load packages, including fredr - fetching data from FRED
if(!require ( "pacman" , quietly = TRUE ) ) {
   install.packages("pacman")
   library(pacman)
   }
if(!require ( "fredr" , quietly = TRUE ) ) {
  install.packages("fredr")
  library(fredr)
  }
library(tidyverse)

# Set your FRED API key - obtain from https://fredaccount.stlouisfed.org/apikey
fredr_set_key("ec84f06f1b178b454f2923dc06f22591")

# Fetch Chile and Argentina Gini Coefficients

chile_gini <- fredr(series_id = "SIPOVGINICHL" , observation_start = as.Date("1987-01-01"))

argentina_gini <- fredr(series_id = "SIPOVGINIARG" , observation_start = as.Date("1987-01-01"))

# Fetch commodity prices

copper_price <- fredr(series_id = "PCOPPUSDM" , observation_start = as.Date("1992-01-01"))

aluminium_price <- fredr(series_id = "PALUMUSDM" , observation_start = as.Date("1992-01-01"))

zinc_price <- fredr(series_id = "PZINCUSDM" , observation_start = as.Date("1992-01-01"))

# Tidy the data for the first plot

arg_gini <- argentina_gini %>% 
  rename(
    argentina = value
  )

chil_gini <- chile_gini %>% 
  rename(
    chile = value
  )

# Merge Chile and Argentina and select relevant columns
merge01 <- arg_gini %>% 
  left_join(
    chil_gini,
    by = "date"
  ) %>% 
  select(date, argentina, chile)

# Pivot longer
inequality <- merge01 %>% 
  pivot_longer(
    cols = argentina:chile,
     names_to = "country",
     values_to = "gini"
  )

# Tidy the data for Plot 2

# Rename "value" column to commodity
copper_price <- copper_price %>% 
  rename(
    copper_price = value
  )

aluminium_price <- aluminium_price %>% 
  rename(
    aluminium_price = value
  )

zinc_price <- zinc_price %>% 
  rename(
    zinc_price = value
  )

# Merge commodity prices

merge02 <- copper_price %>% 
  left_join(
    aluminium_price,
    by = "date"
  ) %>% 
  select(date, copper_price, aluminium_price)

merge03 <- merge02 %>% 
  left_join(
    zinc_price,
    by = "date"
  ) %>% 
  select(date, copper_price, aluminium_price, zinc_price)

# Pivot longer
commodity_prices <- merge03 %>% 
  pivot_longer(
    cols = copper_price:zinc_price,
    names_to = "commodity",
    values_to = "price"
  )

# Plot 1: Chile and Argentina Gini Coefficients
ggplot(inequality,
       aes(x = date, y = gini, colour = country)) +
  geom_point() +
  geom_smooth() +
  labs( title = "Chile and Argentina Gini Coeffecients", y = "Gini", x = "Year")

#Plot 2: Commodity Prices
ggplot(commodity_prices,
       aes(x = date, y = price, colour = commodity)) +
  geom_line() +
  labs( title = "Commodity Prices", y = "USD per Metric Ton", x = "Year")

