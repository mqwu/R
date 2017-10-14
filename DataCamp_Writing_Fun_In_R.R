#----------------------------------------------------------
# Writing Functions in R (DataCamp course)
#----------------------------------------------------------
# seq_along: loop each col

for (i in seq_along(df)) {
  print(median(df[[i]]))
}

## allocate space for faster performance
# Create new double vector: output
output <- vector("double", ncol(df))

# Alter the loop
for (i in seq_along(df)) {
  # Change code to store result in output
  output[i] <- median(df[[i]])
}

# Define example vector x
x <- 1:10 


# Use the function template to create the rescale01 function
rescale01 <- function(x) {
  # body
  rng <- range(x, na.rm = TRUE) 
  (x - rng[1]) / (rng[2] - rng[1])
}

# Test your function, call rescale01 using the vector x as the argument
rescale01(x)


## use message() to highlight output info
replace_missings <- function(x, replacement) {
  is_miss <- is.na(x)
  x[is_miss] <- replacement
  
  # Rewrite to use message()
  #cat(sum(is_miss), replacement, "\n")
  message(sum(is_miss), " missings replaced by the value ", replacement)
  x
}

# Check your new function by running on df$z
replace_missings(df$z, replacement = 0)

## functional programming
# give equal weight to verbs and nouns
# abstract away the details of implementation
# functions + vectors

# Add a second argument called power
f <- function(x, power) {
    # Edit the body to return absolute deviations raised to power
    abs(x - mean(x))^power
}

# function can be argument as well
col_summary <- function(df, fun) {
  output <- vector("numeric", length(df))
  for (i in seq_along(df)) {
    output[[i]] <- fun(df[[i]])
  }
  output
}

# library(purrr)
# Find the 5th percentile of each column, excluding missing values
map_dbl(planes, quantile, probs=0.05, na.rm=T)

# Find the columns that are numeric
map_lgl(df3, is.numeric)

# Find the type of each column
map_chr(df3, typeof)

# Find a summary of each column
map(df3, summary)

## shortcut to write a function .f
# Rewrite to call an anonymous function
map(cyl, ~lm(mpg ~ wt, data=.))

# Save the result from the previous exercise to the variable models
models <- map(cyl, ~ lm(mpg ~ wt, data = .))
# Use map and coef to get the coefficients for each model: coefs
coefs <- map(models, coef)
# Shortcut  when .f is [[
map_dbl(coefs, "wt")

coefs <- map(models, coef)
# use map_dbl with the numeric shortcut to pull out the second element
map_dbl(coefs, 2)

## map over two argument
# Initialize n
n <- list(5, 10, 20)

# Create a list mu containing the values: 1, 5, and 10
mu <- list(1, 5, 10)

# Edit to call map2() on n and mu with rnorm() to simulate three samples
map2(n, mu, rnorm)

## map over p arguments
# Initialize n and mu
n <- list(5, 10, 20)
mu <- list(1, 5, 10)

# Create a sd list with the values: 0.1, 1 and 0.1
sd <- list(0.1, 1, 0.1)

# Edit this call to pmap() to iterate over the sd list as well
pmap(list(n, mu, sd), rnorm)

# to be safe, name the elements of the argument list
pmap(list(mean=mu, n=n, sd=sd), rnorm)

## loop over functions
# Define list of functions
f <- list("rnorm", "runif", "rexp")

# Parameter list for rnorm()
rnorm_params <- list(mean = 10)

# Add a min element with value 0 and max element with value 5
runif_params <- list(min=0, max=5)

# Add a rate element with value 5
rexp_params <- list(rate=5)

# Define params for each function
params <- list(
  rnorm_params,
  runif_params,
  rexp_params
)

# Call invoke_map() on f supplying params as the second argument
invoke_map(f, n = 5)

# Define list of functions
f <- list("rnorm", "runif", "rexp")

# Parameter list for rnorm()
rnorm_params <- list(mean=10)

# Add a min element with value 0 and max element with value 5
runif_params <- list(min=0, max=5)

# Add a rate element with value 5
rexp_params <- list(rate=5)

# Define params for each function
params <- list(
  rnorm_params,
  runif_params,
  rexp_params
)

# Call invoke_map() on f supplying params as the second argument
invoke_map(f, params, n=5)

##pipe
# Define models (don't change)
models <- mtcars %>% 
  split(mtcars$cyl) %>%
  map(~ lm(mpg ~ wt, data = .))

# Rewrite to be a single command using pipes, last chain results become the first argument of next function
models %>% map(summary) %>% map_dbl("r.squared")

# safely() is an adverb; it takes a verb and modifies it. That is, it takes a function as an argument and it returns 
# a function as its output. The function that is returned is modified so it never throws an error 
# (and never stops the rest of your computation!).
# Create safe_readLines() by passing readLines() to safely()
safe_readLines <- safely(readLines)

# Call safe_readLines() on "http://example.org"
safe_readLines( "http://example.org")

# Call safe_readLines() on "http://asdfasdasdkfjlda"
safe_readLines("http://asdfasdasdkfjlda")     
       
# Define safe_readLines()
safe_readLines <- safely(readLines)

# Use the safe_readLines() function with map(): html
html <- map(urls, safe_readLines)

# Call str() on html
str(html)

# Extract the result from one of the successful elements
html[["example"]][["result"]]
# Extract the error from the element that was unsuccessful
html[["asdf"]][["error"]]

      # purrr provides a function transpose() that reshapes a list so the inner-most level becomes the outer-most level. 
      # In otherwords, it turns a list-of-lists "inside-out".
       # Define save_readLines() and html
safe_readLines <- safely(readLines)
html <- map(urls, safe_readLines)

# Examine the structure of transpose(html)
str(transpose(html))

# Extract the results: res
res <- transpose(html)[["result"]]

# Extract the errors: errs
errs <- transpose(html)[["error"]]
       
       # Initialize some objects
safe_readLines <- safely(readLines)
html <- map(urls, safe_readLines)
res <- transpose(html)[["result"]]
errs <- transpose(html)[["error"]]

# Create a logical vector is_ok
is_ok <- map_lgl(errs, is_null)

# Extract the successful results
res[is_ok]

# Extract the input from the unsuccessful results
res[!is_ok]
       
       
## walk for side effect(we do not care about return values)
# Define list of functions
f <- list(Normal = "rnorm", Uniform = "runif", Exp = "rexp")

# Define params
params <- list(
  Normal = list(mean = 10),
  Uniform = list(min = 0, max = 5),
  Exp = list(rate = 5)
)

# Assign the simulated samples to sims
sims <- invoke_map(f, params, n = 50)

# Use walk() to make a histogram of each element in sims
 walk(sims, hist)

       # Replace "Sturges" with reasonable breaks for each sample
breaks_list <- list(
  Normal = seq(6, 16, 0.5),
  Uniform = seq(0, 5, 0.25),
  Exp = seq(0, 1.5, 0.1)
)

# Use walk2() to make histograms with the right breaks
walk2(sims, breaks_list, hist)
       
# good example
# Increase sample size to 1000
sims <- invoke_map(f, params, n = 1000)

# Compute nice_breaks (don't change this)
nice_breaks <- map(sims, find_breaks)

# Create a vector nice_titles
nice_titles <- c("Normal(10, 1)", "Uniform(0, 5)", "Exp(5)")

# Use pwalk() instead of walk2()
pwalk(list(x=sims, breaks=nice_breaks, main=nice_titles), hist, xlab = "")
       
# walk returns the original input obj
# Pipe this along to map(), using summary() as .f
sims %>%
  walk(hist) %>%
  map(summary)
 
       
## hidden argument
# Read in the swimming_pools.csv to pools
pools <- read.csv("swimming_pools.csv")

# Examine the structure of pools
str(pools)

# Change the global stringsAsFactor option to FALSE
options(stringsAsFactors = FALSE)

# Read in the swimming_pools.csv to pools2
pools2 <- read.csv("swimming_pools.csv")

# Examine the structure of pools2
str(pools2)       
