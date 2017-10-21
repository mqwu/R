#----------------------------------------------------------
# Stirng Manipulation with stringr (DataCamp course)
#----------------------------------------------------------

# " inside quote
"hi!"  # normal
' "hi!" ' # have "" inside quote, use ' '
" I'd say \"hi!\" "  # have both ' and "" inside quote, use \" inside quote

writeLines(string) # to see the string it represents (do not use print, it will shows \")
writeLines("hello\n\U1F30D")
writeLines("hello\\")  # escape \ for \


library(stringr)
# most fun starts with str_

str_c()  
# similar to paste0, concatenate, default sep="" (not space)
# str_c() propagates missing values. That means combining any strings with a missing value will result in another missing value.
str_c(my_toppings_str, collapse=",")  # one NA will make entire results NA when collapse


str_length()
# Confirm str_length() works with factors
head(str_length(factor(boy_names)))


str_sub()
# Extract the last letter in boy_names, then tabulate
boy_last_letter <- str_sub(boy_names,-1, -1)
table(boy_last_letter)
# modify a string
str_sub(x, 3, 3) <- "X"

# Extract the first letter in girl_names, then tabulate
girl_first_letter <- str_sub(girl_names, 1,1)
table(girl_first_letter)


## hunting for matches
str_detect(string=a, pattern="yy"))
str_subset(string=a, pattern=fixed("xx"))
str_extract(string, pattern)  # 1. return the same length as string, 2. only return patterns
str_count(string=a, pattern=fixed("xx"))


# Look for pattern "zz" in boy_names
contains_zz <- str_detect(string=boy_names, pattern=fixed("zz"))
# Examine str() of contains_zz
str(contains_zz)
# How many names contain "zz"?
sum(contains_zz)
# Which names contain "zz"?
boy_names[contains_zz]

# Find girl_names that contain "U" and "z"
str_subset(str_subset(girl_names, "U"), "z")


# Count occurrences of "a" in girl_names
number_as <- str_count(girl_names, "a")
# Count occurrences of "A" in girl_names
number_As <- str_count(girl_names, "A")
# Histograms of number_as and number_As
hist(number_as)
hist(number_As)
# Find total "a" + "A"
total_as <- number_as + number_As
# girl_names with more than 4 a's
girl_names[total_as>4]


# difference
 # extract part of a strings for each element
string_sub(string, start, end) 

#  returns the elements of a character vector that match a regular expression (similar to grep() with value = TRUE)`.
string_subset(strings, pattern) 

# return same length as string, only return patterns
# extracts text corresponding to the first match, returning a character vector. 
string_extract(string, pattern) 
# extracts all matches and returns a list of character vectors.
str_extract_all() 


##word and letter counts
# Split lines into words
words <- str_split(lines, " ")

# Number of words per line
lapply(words, length)
  
# Number of characters in each word
word_lengths <- lapply(words, str_length)
  
# Average word length per line
lapply(word_lengths, mean)
