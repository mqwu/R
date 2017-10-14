#----------------------------------------------------------
#Joining data in R with dplyr (DataCamp course)
#----------------------------------------------------------
# dplyr preserve order of rows in your raw data

## mutating join, add one df to another df
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

# Check two sets
setequal(bands2, bands3)
