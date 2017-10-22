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

# examples
https://cran.r-project.org/web/packages/stringr/vignettes/stringr.html

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


str_replace(str, pattern, replacement)
str_replace_all(str, pattern, replacement)


ids <- c("ID#: 192", "ID#: 118", "ID#: 001")

# Replace "ID#: " with ""
id_nums <- str_replace(ids, "ID#: ", "")

# Turn id_nums into numbers
id_ints <- as.numeric(id_nums)
  
# Some (fake) phone numbers
phone_numbers <- c("510-555-0123", "541-555-0167")

# Use str_replace() to replace "-" with " "
str_replace(phone_numbers, "-", " ")

# Use str_replace_all() to replace "-" with " "
str_replace_all(phone_numbers, "-", " ")

# Turn phone numbers into the format xxx.xxx.xxxx
str_replace_all(phone_numbers, "-", ".")


# --- Task 1 ----
# Define some full names
names <- c("Diana Prince", "Clark Kent")

# Split into first and last names
names_split <- str_split(names, " ", simplify=TRUE)

# Extract the first letter in the first name
abb_first <- str_sub(names_split[,1], 1,1)

# Combine the first letter ". " and last name
str_c(str_c(abb_first, ". "), names_split[,2])

# --- Task 2 ----
# Use all names in babynames_2014
all_names <- babynames_2014$name

# Get the last two letters of all_names
last_two_letters <- str_sub(all_names, -2, -1)

# Does the name end in "ee"?
ends_in_ee <- str_detect(last_two_letters, pattern=fixed("ee"))

# Extract rows and "sex" column
sex <- babynames_2014[ends_in_ee,]$sex

# Display result as a table
table(sex)

## Regular expression
library(rebus) # build Regular Expressions in a Human Readable Way
library(stringr)

# Some strings to practice with
x <- c("cat", "coat", "scotland", "tic toc")

# Run me
str_view(x, pattern = START %R% "c") # start of string %R% = then

# Match the strings that start with "co" 
str_view(x, pattern = START %R% "co")  # help you check results

# Match the strings that end with "at"
str_view(x, pattern =  "at" %R% END)

# Match the strings that is exactly "cat"
str_view(x, pattern = START %R% "cat" %R% END)  # end of string

## Wild card
# any char
ANY_CHAR
x <- c("cat", "coat", "scotland", "tic toc")

# Match any character followed by a "t"
str_view(x, pattern = ANY_CHAR %R% "t")

# Match a "t" followed by any character
str_view(x, pattern = "t" %R% ANY_CHAR)

# Match two characters
str_view(x, pattern = ANY_CHAR %R% ANY_CHAR)

# Match a string with exactly three characters
str_view(x, pattern = START %R% ANY_CHAR %R% ANY_CHAR %R% ANY_CHAR %R% END)

# q followed by any character
pattern <- "q" %R% ANY_CHAR

# Test pattern 
str_view(c("Quentin", "Kaliq", "Jacques",  "Jacqes"), pattern)  

# Find names that have the pattern
names_with_q <- str_subset(boy_names, pattern)
length(names_with_q)

# Find part of name that matches pattern
part_with_q <- str_extract(boy_names, pattern)
table(part_with_q)

# Did any names have the pattern more than once?
count_of_q <- str_count(boy_names, pattern)
table(count_of_q)
# Which babies got these names?
with_q <- str_detect(boy_names, pattern)

# What fraction of babies got these names?
mean(with_q)

## alternative 
or(A, B)

# Match Jeffrey or Geoffrey
whole_names <- or("Jeffrey", "Geoffrey")
str_view(boy_names, pattern = whole_names, 
  match = TRUE)

# Match Jeffrey or Geoffrey, another way
common_ending <- or("Je", "Geo") %R% "ffrey"
str_view(boy_names, pattern = common_ending, 
  match = TRUE)

# Match with alternate endings
by_parts <- or("Je", "Geo") %R% "ff" %R% or("ry", "ery", "rey", "erey")
str_view(boy_names, 
  pattern = by_parts, 
  match = TRUE)

# Match names that start with Cath or Kath
ckath <- START %R% or("Cath", "Kath")
str_view(girl_names, pattern = ckath, match = TRUE)


## char classes
"match one (and only one) of the following characters"
char_class()
"any single character that isn't one of the following"

# Create character class containing vowels
vowels <- char_class("aeiouAEIOU")

# Print vowels
print(vowels)

# See vowels in x with str_view()
str_view(x, vowels)

# See vowels in x with str_view_all()
str_view_all(x, vowels)

# Number of vowels in boy_names
num_vowels <- str_count(boy_names, vowels)
mean(num_vowels)

# Proportion of vowels in boy_names
name_length <- str_length(boy_names)
mean(num_vowels/name_length)

#The rebus functions one_or_more(), zero_or_more() and optional() can be used to wrap parts of a regular expression 
#to allow a pattern to match a variable number of times.
negated_char_class()

# Vowels from last exercise
vowels <- char_class("aeiouAEIOU")

# Use `negated_char_class()` for everything but vowels
not_vowels <- negated_char_class("aeiouAEIOU")

# See names with only vowels
str_view(boy_names, 
  pattern = START %R% one_or_more(vowels) %R% END, 
  match = TRUE)

# See names with no vowels
str_view(boy_names, 
  pattern = START %R% one_or_more(not_vowels) %R% END, 
  match = TRUE)
