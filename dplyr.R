
# display data
flights <- tbl_df(hflights)  # convert to loca ldata frame for efficient display

print(flight, n=20)

data.frame(head(flights))  # convert to a normal data frame to see all of vars

glimpse(mammals)  # look at data in a transpose view

# filter rows
fliter(flights, Month==1, DayofMonth==1)  # M==1& DayofM==1
fliter(flights, Month==1|Month==2)  # M==1 or 2
fliter(flights, Month %in% c(1,2))  # M==1 or 2

# select cols
select(flight, v1, v2, v3)
select(flight, v1:v3)
select(flight, v1:v10, contains("Taxi"), contains("Deylay"))
# starts_with, ends_with, matches for regular expressions

# chain
x1 <- 1:5; x2 <- 2:6
(x1-x2)^2 %>% sum() %>% sqrt()  # chain

# arrange
fligth %>% 
	select(v1, v2) %>%
	arrange(desc(v2))
		
# mutate
flight <- fligth %>% 
		select(v1, v2) %>%
		mutate(v3=v1+v2)  # not necessary select variables first

# summarise			
## summarise avg delay by destination
flights %>%
	group_by(Dest) %>%
	summarise(avg_delay = mean(ArrDelay, na.rm=TRUE))

## n()	
flight %>%
	group_by(Month, DayofMonth) %>%
	summarise(flight_count=n()) %>%  #n()
	arrange(desc(flight_count))

# tally (equivalent to above, count each group)
flight %>%
	group_by(Month, DayofMonth) %>%
	tally(sort=TRUE)  # create a variable named n

# n_distinct
flights %>%
	group_by(Dest) %>%
	summarise(flight_count=n(), plane_count=n_distinct(TailNum))
		
# summarise_each 
# allows to apply the same summary function to multiple cols
flights %>%
	group_by(v1) %>%
	summarise_each(funs(mean), Cancelled, Diverted)
		
flights %>%
	group_by(UniqueCarrier) %>%
	summarise_each(funs(min(., na.rm=TRUE), max(., na.rm=TRUE)),
	matches("Delay"))  # variable names contains delay
	
# use other R base function for group
flights %>%
	group_by(Dest) %>%
	select(Cancelled) %>%
	table() %>% 
	head()
	
# window function: take n inputs and returns n values
## min_rank/top_n
flights %>%
	group_by(UniqueCarrier) %>%
	select(Month, DayofMonth, DepDelay) %>%
	filter(min_rank(desc(DepDelay)) <= 2) %>%  # window function = top_n(2)
	arrange(UniqueCarrier, desc(DepDelay))
	
## lag
flights %>%
	group_by(Month) %>%
	summarise(flight_count=n()) %>%
	mutate(change= flight_count - lag(flight_count))

flights %>%
	group_by(Month) %>%
	tally() %>%  # create count varaible "n"
	mutate(change= n - lag(n))
	
# sampling
flights %>% sample_n(5)
flights %>% sample_frac(0.25, replace=TRUE)


