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

# Extract the first letter in girl_names, then tabulate
girl_first_letter <- str_sub(girl_names, 1,1)
table(girl_first_letter)
