# Essex Summer School in Social Science Data Analysis
# 1X Introduction to R
# Phil Swatton
# Sunday 9th July 2023, 11am-5pm BST
# File 05: extras


## Packages
library(tidyverse)
library(readstata13)
library(texreg)


## Data
ches <- read.dta13("data/1999-2019_CHES_dataset_means(v3).dta")


# 1 Writing Functions ----

# Sometimes, the functions available either in base R or in the many packages
# out there don't quite cover your needs. A common case might be a specific recode
# that you apply to many of your variables. Instead of just re-writing this recode
# over and over again, you can write it into a function. The benefit of this is that
# if you make mistakes, or need to make changes, you can adjust it in one place, instead
# of copy/pasting the issue throughout your code.

# To write functions in R, we use the special function() function to declare that we're
# making a function. We supply the arguments we want this function to have as inputs
# to the function() function.

# For example, a function to calculate an arbitrary root:

root <- function(x, r) {
  out <- x^(1/r)
  return(out)
}

# Here, we assign the output of function() to the name we want to give our
# new function, in this case to root. When you think about it, this isn't any different
# to anything we've already seen already - functions can be thought of as values assigned
# to names too!

# The special return() function inside declares what the function puts out in the end.
# You don't always need it (if we hadn't assigned to 'out', the output would be returned),
# but it's good practice to include it.

# Once we've assigned our function, we can use it:
root(27, 3)

# This is (a big part of) how R packages are made!





# 2 Loops ----

# Loops are a common programming construct in almost every programming language.
# Like functions, they're very useful for avoiding the need to copy and paste.

## 2.1 Base R loops ----

# The basic loop in R uses the special for() function and in keyword:

for (i in 1:10) {
  print(i)
  Sys.sleep(0.5) #makes the loop wait half a second
}

# We create a variable called i in the bracket, which draws on
# the values in the vector after 'in'.

# The loop then sits inside the curly brackets, where the 'i' variable will
# exist for whatever purposes you want it for.

# The loop then does its operation on each 'i' sequentially



## 2.2 Apply -----

# The apply family of functions is a quite powerful version of loops:
?apply

# Apply works on a dataframe or matrix. For example:
apply(ches |> select_if(is.numeric),
      MARGIN=2,
      FUN=mean,
      na.rm=T)

# It takes a dataframe as the first input.

# The margin specifies whether to apply the operation by row (1)
# or column (2).

# The fun argument takes a function as input. In this case, I supplied
# the mean function.
# The ... passes arguments to the function - in this case na.rm=T.

# lapply is the equivalent for lists:
?lapply



## 2.3 map ----

# The map family of functions is the tidyverse answer to 
# apply.

# For example, using map to create 3 dataframes and put them in a list:
countries <- c(3,6,11)
filter_country <- function(c) {
  out <- ches |> filter(country == c) |> select(party, year, lrgen, lrecon, eu_position, galtan)
  return(out)
}
map(countries, filter_country)



## 2.4 Use example ----

# Let's take that list of dataframes from earlier
df_list <- map(countries, filter_country)

# Let's now pass it through map again to make a list of regression results
m <- lrgen ~ lrecon + eu_position + galtan
result_list <- map(df_list, function (x) { #notice I'm writing my function INSIDE map this time!
  result <- lm(m, x)
  return(result)
})

# Pass the list to screenreg:
screenreg(result_list,
          custom.model.names = c("Germany", "France", "UK"))



