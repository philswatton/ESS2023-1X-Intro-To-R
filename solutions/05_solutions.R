# Exercise Solutions for File 05 ----


# Run these lines of code:
library(tidyverse)
library(readstata13)
library(texreg)
ches <- read.dta13("data/1999-2019_CHES_dataset_means(v3).dta")
country_indices <- c(3,6,11)



# 1) This is a copy paste of the function used above. Add the 'eu_position' and
#    'galtan' variables to the select() call
filter_country <- function(c) {
  out <- ches |> 
    filter(country == c) |> 
    select(party, year, lrgen, lrecon, eu_position, galtan)
  return(out)
}


# 2) Run 'country_indices' and your new 'filter_country' function through
#    'map', and store the output. You want it to be a list of 3 dataframes.
dlist <- map(country_indices, filter_country)


# 3) Build a model (don't run it yet!) regressing 'lrgen' on 'lrecon', 'eu_position',
#    and 'galtan', to see which dimensions contribute to overall left-right party position.
ches_model <- lrgen ~ lrecon + eu_position + galtan


# 4) Now, run each of the 3 dataframes in your list from question 2 through lm() 
#    along with your model from question 3. Use the 'map' function to do it and output
#    a list of results.
ches_models <- map(dlist, function(x) lm(ches_model, x))


# 5) Finally, run your models through screenreg(). Add model names for each country
headings <- c("Germany", "France", "UK")
screenreg(ches_models, custom.model.names=headings)

# Congratulations, you've now used functions and loops to run 3 separate models
# in an efficient way!