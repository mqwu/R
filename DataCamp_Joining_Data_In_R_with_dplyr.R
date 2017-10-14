#----------------------------------------------------------
#Joining data in R with dplyr (DataCamp course)
#----------------------------------------------------------
# dplyr preserve order of rows in your raw data

## mutating joins
#add one df to another df
# left_join: keep every row of primary df, and add cols of augment df with matching rows
left_join(df_primary, df_augment, by = "key")
left_join(df_primary, df_augment, by = c("key1", "key2"))

right_join(df_augment, df_primary, by = "key")

# inner_join: return rows appear in both df (exclusive join)
inner_join(df1, df2, by = "key")

# full_join: return every rows appear in either df (inclusive join)
full_join(df1, df2, by = "key")

# %>%
bands %>% 
  left_join(artists,  by = c("first", "last")) %>%  # left_join(bands, artists...)
  filter(instrument == "Guitar") %>%
  select(first, last, band)


## filter join
# semi_join: filtering primary df with rows that the key appears in the 2nd df
semi_join(df1, df2, by = "name")  # only keep df1 cols and keep rows that appear in df2

albums %>% 
  # Collect the albums made by a band
  semi_join(bands, by = "band" ) %>% 
  # Count the albums made by a band
  nrow()

# anti_join: filter primary df with rows that does not apprear in 2nd df
anti_join(df1, df2, by = "key")


## set operations
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


