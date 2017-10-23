#----------------------------------------------------------
# Stirng Manipulation with stringr (DataCamp course)
#----------------------------------------------------------

###########################################################
# string basic
###########################################################

## turn number into string
estimate <- 1.34019029100
as.character(estimate)
[1] "1.340190291"
format(estimate, digits = 3, scientific=FALSE)
[1] "1.34"
formatC(estimate, format = "f", digits = 2)  # C language format "f" "e" "g"

# " inside quote
"hi!"  # normal
' "hi!" ' # have "" inside quote, use ' '
" I'd say \"hi!\" "  # have both ' and "" inside quote, use \" inside quote

writeLines(string) # to see the string it represents (do not use print, it will shows \")
writeLines("hello\n\U1F30D")
writeLines("hello\\")  # escape \ for \

# examples
https://cran.r-project.org/web/packages/stringr/vignettes/stringr.html


###########################################################
# stringr
###########################################################
library(stringr)
# most fun starts with str_

## common
str_c()  
str_length()
str_sub()

## hunting for matches
str_detect()  # return logic vector, the same length as string
str_count()   # return 0 1 4 5 vector, the same length as string
str_subset()  # return strings vector with patterns, length <= original string length

## split
str_split(str, pattern="&", simplify=TRUE)  # simplyfy=T return matrix
lapply(str_split(str, pattern="&", simplify=TRUE), length)

## replace matching in string
str_replace(str, pattern, replacement)  # only replace first in each element of a string vector
str_replace_all(str, pattern, replacement)



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


##################################################################
## Regular expression
##################################################################
library(rebus) # build Regular Expressions in a Human Readable Way
library(stringr)

## basic
START
END
exactly()
ANY_CHAR
%R%  # means then

## char class
char_class("Aa")
negated_char_class("Aa") 
str_view(string, pattern)

## repetition
optional()
zero_or_more()
one_or_more()
repeated(pattern, n, m)  # between n and m times {n}{m}

## shortcut
DGT
char_class("0-9")

WRD
char_class("a-zA-Z0-9_")

SPC
a white space



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
# alternative 
or(A, B)

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

## Short cut
DGT # digit

# Hunting phone #
# Take a look at ALL digits
str_view_all(contact, DGT)

# Create a three digit pattern and test
three_digits <- DGT %R% DGT %R% DGT
str_view_all(contact,
  pattern = three_digits)

# Create four digit pattern
four_digits <-  DGT %R% DGT %R% DGT %R% DGT

# Create a separator pattern and test
separator <-  char_class("-.() ")
str_view_all(contact,
  pattern = separator)

# Create phone pattern
phone_pattern <- optional(OPEN_PAREN) %R%
  three_digits %R%
  zero_or_more(separator) %R%
  three_digits %R% 
  zero_or_more(separator) %R%
  four_digits
      
# Test pattern           
str_view(contact, phone_pattern)

# Extract phone numbers
str_extract(contact, phone_pattern)

# Extract ALL phone numbers
str_extract_all(contact, phone_pattern)

# Look for two digits
str_view(narratives, pattern=DGT %R% DGT)

# Pattern to match one or two digits
age <- repeated(DGT, 1, 2)
str_view(narratives, 
  pattern = age)

# Pattern to match units 
unit <- optional(SPC) %R% or("YO", "YR", "MO")

# Test pattern with age then units
str_view(narratives, 
  pattern = age %R% unit)

# Pattern to match gender
gender <- optional(SPC) %R% or("M", "F")

# Test pattern with age then units then gender
str_view(narratives, 
  pattern = age %R% unit %R% gender)

# Extract age_gender, take a look
age_gender <- str_extract(narratives, age %R% unit %R% gender)
age_gender


########################
# Advanced matching
########################
capture()
str_match(str, pattern=capture() %R% ...)
capture(or("A", "B"))  # or is non-capturing groups
# backreferences: for finding repeated patterns or words.
# 1. you need to capture() the part of the pattern you want to reference 2. you refer to it with REF1.
REF1  # up to REF9

# The replacement argument to str_replace() can also include backreferences. 
# This works just like specifying patterns with backreferences, except the capture happens in the pattern argument, 
# and the backreference is used in the replacement argument.

# Build pattern to match words ending in "ING"
pattern <- one_or_more(WRD) %R% "ING" 
str_view(narratives, pattern)

# Test replacement
str_replace(narratives, capture(pattern), 
  str_c("CARELESSLY", REF1, sep = " "))

# One adverb per narrative
adverbs_10 <- sample(adverbs, 10)

# Replace "***ing" with "adverb ***ing"
str_replace(narratives, 
  capture(pattern),
  str_c(adverbs_10, REF1, sep = " "))  

str_replace("Paris in the the spring",
             pattern = SPC %R%
             capture(one_or_more(WRD)) %R%
             SPC %R%
             REF1,
             replacement = str_c(" ", REF1))

# See names with three repeated letters
repeated_three_times <- capture(WRD) %R% REF1 %R% REF1
str_view(boy_names, 
  pattern = repeated_three_times, 
  match = TRUE)

# See names with a pair of repeated letters
pair_of_repeated <- capture(WRD)%R%capture(WRD)%R%REF1%R%REF2
str_view(boy_names, 
  pattern = pair_of_repeated, 
  match = TRUE)

# See names with a pair that reverses
pair_that_reverses <- capture(WRD)%R%capture(WRD)%R%REF2%R%REF1
str_view(boy_names, 
  pattern = pair_that_reverses, 
  match = TRUE)

# See four letter palindrome names
four_letter_palindrome <- exactly(capture(WRD)%R%capture(WRD)%R%REF2%R%REF1)
str_view(boy_names, 
  pattern = four_letter_palindrome, 
  match = TRUE)



#e.g
# Capture part between @ and . and after .
email <- capture(one_or_more(WRD)) %R% 
  "@" %R% capture(one_or_more(WRD)) %R% 
  DOT %R% capture(one_or_more(WRD))

# Check match hasn't changed
str_view(hero_contacts, pattern=email)


# Pull out match and captures
email_parts <- str_match(hero_contacts, email)

# Print email_parts
print(email_parts)

# Save host
host <- email_parts[,3]
host

# View text containing phone numbers
contact

# Add capture() to get digit parts
phone_pattern <- capture(three_digits) %R% zero_or_more(separator) %R% 
           capture(three_digits) %R% zero_or_more(separator) %R%
           capture(four_digits)
           
# Pull out the parts with str_match()
phone_numbers <- str_match(contact, phone_pattern)

# Put them back together
str_c(
  "(",
  phone_numbers[,2],
  ") ",
  phone_numbers[,3],
  "-",
  phone_numbers[,4])

# narratives has been pre-defined
narratives

# Add capture() to get age, unit and sex
pattern <- capture(optional(DGT) %R% DGT) %R%  
  optional(SPC) %R% capture(or("YO", "YR", "MO")) %R%
  optional(SPC) %R% capture(or("M", "F"))

# Pull out from narratives
str_match(narratives, pattern)

# Edit to capture just Y and M in units
pattern2 <- capture(optional(DGT) %R% DGT) %R%  
  optional(SPC) %R% capture(or("Y", "M")) %R% optional(or("O","R")) %R%
  optional(SPC) %R% capture(or("M", "F"))

# Check pattern
str_view(narratives, pattern2)

# Pull out pieces
str_match(narratives, pattern2)


# Replace digits with "X"
str_replace(contact, pattern =DGT, replacement = "X")

# Replace all digits with "X"
str_replace_all(contact, pattern = DGT, replacement = "X")

# Replace all digits with different symbol
str_replace_all(contact, pattern = DGT, 
  replacement = c("X", ".", "*", "_"))

################################
# unicode and pattern matching
################################
"\uxxxx"
"\Uxxxx"
http://www.unicode.org/charts/
http://www.fileformat.info/info/unicode/char/search.htm

# composes characters with combining accents into a single character.
stri_trans_nfc()  

# decomposes character with accents into separate letter and accent characters.
stri_trans_nfd() 


# Things can get tricky when some characters can be specified two ways, 
# for example è, an e with a grave accent, can be specified either with the single code point \u00e8 
# or the combination of a \u0065 and a combining grave accent \u0300. They look the same:

# In Unicode, an accent is known as a diacritic Unicode Property, 
# and you can match it using the rebus value 
UP_DIACRITIC



########################
library(stringi)
########################

# Read play in using stri_read_lines()
earnest <- stri_read_lines("importance-of-being-earnest.txt")
