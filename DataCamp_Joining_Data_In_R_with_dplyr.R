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
            

            
### example
lahmanNames %>%  
  # Bind the data frames in lahmanNames
  bind_rows(.id="dataframe") %>%
  # Group the result by var
  group_by(var) %>%
  # Tally the number of appearances
  tally() %>%
  # Filter the data
  filter(n>1) %>% 
  # Arrange the results
  arrange(desc(n))
            
lahmanNames %>% 
  # Bind the data frames
  bind_rows(.id = "dataframe") %>%
  # Filter the results
  filter(var ==  "playerID") %>% 
  # Extract the dataframe variable
  `$`(dataframe)
            
players <- Master %>% 
  # Return one row for each distinct player
  distinct(playerID, nameFirst, nameLast)     
            
players %>% 
  # Find all players who do not appear in Salaries
  anti_join(Salaries, by = "playerID") %>%
  # Count them
  count()
            
players %>% 
  # Find all players who do not appear in Salaries
  anti_join(Salaries, by = "playerID") %>% 
  # Join them to Appearances
  left_join(Appearances, by = "playerID") %>% 
  # Calculate total_games for each player
  group_by(playerID) %>%
  summarize(total_games=sum(G_all, na.rm=T)) %>%
  # Arrange in descending order by total_games
  arrange(desc(total_games))
            
players %>%
  # Find unsalaried players
  anti_join(Salaries, by = "playerID") %>% 
  # Join Batting to the unsalaried players
  left_join(Batting, by = "playerID") %>% 
  # Group by player
  group_by(playerID) %>% 
  # Sum at-bats for each player
  summarise(total_at_bat = sum(AB, na.rm=T))%>% 
  # Arrange in descending order
  arrange(desc(total_at_bat))
            
# Find the distinct players that appear in HallOfFame
nominated <- HallOfFame %>% 
  distinct(playerID)

nominated %>% 
  # Count the number of players in nominated
  count()

nominated_full <- nominated %>% 
  # Join to Master
  left_join(Master, by = "playerID") %>% 
  # Return playerID, nameFirst, nameLast
  select(playerID, nameFirst, nameLast)
            
# Find distinct players in HallOfFame with inducted == "Y"
inducted <- HallOfFame %>% 
  filter(inducted == "Y") %>% 
  distinct(playerID)

inducted %>% 
  # Count the number of players in inducted
  count()

inducted_full <- inducted %>% 
  # Join to Master
  left_join(Master, by = "playerID") %>% 
  # Return playerID, nameFirst, nameLast
  select(playerID, nameFirst, nameLast)
            
# Tally the number of awards in AwardsPlayers by playerID
nAwards <- AwardsPlayers %>% 
  group_by(playerID) %>% 
  tally()

nAwards %>% 
  # Filter to just the players in inducted 
  semi_join(inducted) %>% 
  # Calculate the mean number of awards per player
  summarise(avg_n = mean(n,na.rm=T))

nAwards %>% 
  # Filter to just the players in nominated 
  semi_join(nominated, by = "playerID") %>% 
  # Filter to players NOT in inducted 
  anti_join(inducted, by = "playerID") %>% 
  # Calculate the mean number of awards per player
  summarise(avg_n = mean(n, na.rm=T))
            
# Find the players who are in nominated, but not inducted
notInducted <- nominated %>% 
  setdiff(inducted)

Salaries %>% 
  # Find the players who are in notInducted
  semi_join(notInducted, by =  "playerID") %>% 
  # Calculate the max salary by player
  group_by(playerID) %>% 
  summarise(max_salary = max(salary)) %>% 
  # Calculate the average of the max salaries
  summarise(avg_salary = mean(max_salary, na.rm=T))
  

# Repeat for players who were inducted
Salaries %>% 
   semi_join(inducted, by =  "playerID") %>% 
  group_by(playerID) %>% 
  summarise(max_salary = max(salary)) %>% 
  summarise(avg_salary = mean(max_salary, na.rm=T))
