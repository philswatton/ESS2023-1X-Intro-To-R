# Essex Summer School in Social Science Data Analysis
# 1X Introduction to R
# Phil Swatton
# Sunday 9th July 2023, 11am-5pm BST
# File 05: functions, loops, programming


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




# 2 if else ----

# We've already seen the vectorised ifelse() in base R and tidyverse's
# case_when() functions. However, there is also a global if() function.

# This takes a *single* boolean value or test, and if TRUE it does whatever
# is inside the curly brackets:

if (TRUE) {
  print("Do this!")
}

if (FALSE) {
  print("Don't do this!")
}

# Sometimes, you want to do something else if the first part isn't true.
# For instance:

if (2 > 4) {
  print("2 is greater than 4")
} else {
  print("2 is less than or equal to 4")
}

# You can chain multiple if/else statements:

if (2 > 4) {
  print("2 is greater than 4")
} else if (2 == 4) {
  print("2 is equal to 4")
} else {
  print("2 is less than 4")
}

# This type of command flow can be very useful in writing functions.
# It can also be useful for other tasks, like making sure a particular folder 
# exists (e.g. a 'results' folder before saving your latex tables):

if (!"my_folder" %in% dir()) {
  dir.create("my_folder")
}



# 3 Loops ----

# Loops are a common programming construct in almost every programming language.
# Like functions, they're very useful for avoiding the need to copy and paste.

# Where functions define an operation that we then apply to data, loops iteratively
# perform the same action over and over again.

# Combining them goes a long way:

## 3.1 Base R loops ----

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



## 3.2 Apply -----

# The apply family of functions is a quite powerful version of loops:
?apply

# Apply works on a dataframe or matrix. For example:
apply(ches |> select_if(is.numeric),
      MARGIN=2,
      FUN=mean,
      na.rm=T)

# It takes a dataframe (or matrix) as the first input.

# The margin argument specifies whether to apply the operation by row (1)
# or column (2).

# The fun argument takes a function as input. In this case, I supplied
# the mean function.

# The ... passes extra arguments to the function - in this case na.rm=T.

# lapply is the equivalent for lists:
?lapply



## 3.3 map ----

# The map family of functions is the tidyverse's answer to apply/lapply.

# Map will always work over either the elements of a list/vector, or the columns
# of a dataframe.

# Let's take the example of subsetting 3 countries' data from the CHES.
# From reading the documentation, I know that 3 is Germany, 6 is France, and 11 is the UK.
country_indices <- c(3,6,11)

# I can then write a function that filters for the countries I want, and selects
# the variables I want before returning the output, given an input country index.

# Let's read it bit by bit:

filter_country <- function(c) { #c is the input of the function
  out <- ches |> #store the output of the pipe chain in 'out'
    filter(country == c) |> #filter for country equals c
    select(party, year, lrgen, lrecon) #select these variables
  return(out) #return out
}

# Running map over the indices with this function does this for me:
map(country_indices, filter_country)

# There are many map functions. Their suffixes determine what kind of data
# you get back. For instance, map_dfr takes the outputs, then rowbinds them
# to form a single dataset:
map_dfr(country_indices, filter_country)



## 3.4 Example Use Case ----

# CHES has a lot of expert measurements on different ideological dimensions.
# But what is the relationship between them? Let's look at general left-right
# and economic left-right.

# Let's take the list of CHES dataframes from earlier
df_list <- map(country_indices, filter_country)

# Let's write a model we'd like to run for each country:
m <- lrgen ~ lrecon

# We can now use map again. Notice this time I'm writting the 
# function inside map without ever assigning it to a name. This
# is a programming concept called 'anonymous functions'. apply() allows
# for this behaviour too.
result_list <- map(df_list, function (x) { #notice I'm writing my function INSIDE map this time!
  result <- lm(m, x)
  return(result)
})

# Pass the list to screenreg:
screenreg(result_list,
          custom.model.names = c("Germany", "France", "UK"))




# That's all folks!




# Exercises ----


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
    select(party, year, lrgen, lrecon) #HINT: this is the only line you need to edit
  return(out)
}


# 2) Run 'country_indices' and your new 'filter_country' function through
#    'map', and store the output. You want it to be a list of 3 dataframes.


# 3) Build a model (don't run it yet!) regressing 'lrgen' on 'lrecon', 'eu_position',
#    and 'galtan', to see which dimensions contribute to overall left-right party position.


# 4) Now, run each of the 3 dataframes in your list from question 2 through lm() 
#    along with your model from question 3. Use the 'map' function to do it and output
#    a list of results.


# 5) Finally, run your models through screenreg(). Add model names for each country

# Congratulations, you've now used functions and loops to run 3 separate models
# in an efficient way!






