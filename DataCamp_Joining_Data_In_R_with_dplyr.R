#----------------------------------------------------------
#Joining data in R with dplyr (DataCamp course)
#----------------------------------------------------------
# dplyr preserve order of rows in your raw data

### mutating joins
#add one df to another df
# left_join: keep every row of primary df, and add cols of augment df with matching rows
left_join(df_primary, df_augment, by = "key")
left_join(df_primary, df_augment, by = c("key1", "key2"))
merge(df_primary, df_augment, by = "key", all.x=T, all.y=F)


right_join(df_augment, df_primary, by = "key")
merge(df_primary, df_augment, by = "key", all.x=F, all.y=T)

# inner_join: return rows appear in both df (exclusive join)
inner_join(df1, df2, by = "key")
merge(df_primary, df_augment, by = "key", all=F)

# full_join: return every rows appear in either df (inclusive join)
full_join(df1, df2, by = "key")
merge(df_primary, df_augment, by = "key", all=T)

# %>%
bands %>% 
  left_join(artists,  by = c("first", "last")) %>%  # left_join(bands, artists...)
  filter(instrument == "Guitar") %>%
  select(first, last, band)


### filter join
# semi_join: filtering primary df with rows that the key appears in the 2nd df
semi_join(df1, df2, by = "name")  # only keep df1 cols and keep rows that appear in df2

albums %>% 
  # Collect the albums made by a band
  semi_join(bands, by = "band" ) %>% 
  # Count the albums made by a band
  nrow()

# anti_join: filter primary df with rows that does not apprear in 2nd df
anti_join(df1, df2, by = "key")  # only keep df1 cols and keep rows that does not appear in df2


### set operations
union(df1, df2)
intersect(df1, df2)
setdiff(df1, df2)  # return rows only appear in df1

aerosmith %>% 
  # Create the new dataset using a set operation
  union(greatest_hits) %>% 
  # Count the total number of songs
  nrow()

# compare sets
setequal(df1, df2)  # regardless of order of rows
identical(df1, df2)  # considering order of rows

### assembling data
## bind dataset
# base
rbind()
cbind()

# dplyr
# faster, return a tibble, can handle list of dfs, .id argument
bind_rows()
bind_cols()  # not matching any key!! physical bind

bind_rows(drug_a=df.a, drug_b=df.b, .id = "drug")

jimi %>%  # jimi have 3 lists, each one is a df
  # Bind jimi into a single data frame
  bind_rows(.id="album") %>% 
  # Make a complete data frame
  left_join(discography, by = "album" )

data_frame()  # vs. data.frame()
# will not coerces strings to factors.
# will not change your column names
# never adds row names
# evaluates its arguments lazily and in order.
# Make combined data frame using data_frame()
data_frame(year = hank_year, song = hank_song, peak = hank_peak) %>% 
    filter(peak == 1)

as_data_frame()
# Convert the hank list into a data frame
as_data_frame(hank) %>% 
    filter(peak == 1)


## data type 6
typeof()
logical
character
integer
double
complex
raw

## class of data for catergorical data (2nd attribute)
factor
# Factors are stored as integers, 
# and have labels associated with these unique integers. 
# While factors look (and often behave) like character vectors, they are actually integers under the hood, 
# and you need to be careful when treating them like strings.
y <- factor(c(5, 6, 7, 6)
unclass(y)
as.character(y)  # shows the level of y
as.numeric(y)  # shows the integer representaion of y
as.numeric(as.character(y))  # two step process to convert factor to numeric!!    

# dplyr for combining different type
# coerce two factors to a string if the factors have different levels.
# coerce two factors to a string if the factors have the same levels in a different order.
# return an error instead of coercing logicals, integers, and numerics to a character.            
            
### advance join
# missing key values
filter(!is.na(key))

# missing key cols (sometimes key is the row names)
rownames_to_column(df, var = "name")

# duplicate key values
# will have duplicate mathes

# mismatched keys
left_join(df1, df2, by = c("name1"="name2"))
            
# conflicting vars
left_join(df1, df2, by = c("name"), suffix = c("1", "2"))
            
### join multiple tables
library(purrr)
lists <- list(df1, df2, df3)
reduce(lists, left_join, by = "name")  # apply a function in an iterative fashion to many datasets
            
