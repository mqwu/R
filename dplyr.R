
# display data
flights <- tbl_df(hflights)  # convert to loca ldata frame for efficient display

print(flight, n=20)

data.frame(head(flights))  # convert to a normal data frame to see all of vars

glimpse(mammals)  # look at data in a transpose view

# filter rows
fliter(flights, Month==1, DayofMonth==1)  # M==1& DayofM==1
fliter(flights, Month==1|Month==2)  # M==1 or 2
fliter(flights, Month %in% c(1,2))  # M==1 or 2
fliter(flights, hour >= 0, hour <= 5)  # common represent "And"
fliter(flights, delay_time >= 2*delay_min)  # common represent "And"
fliter(flights, !is.na(dep_delay))  # filter na rows

# select cols
select(flight, v1, v2, v3)
select(flight, v1:v3)
select(flight, v1:v10, contains("Taxi"), contains("Deylay"))
select(flight, ends_with("delay"))
# starts_with, ends_with, contains, matches, num_range, one_of("x","y","z"), everything()


# chain
x1 <- 1:5; x2 <- 2:6
(x1-x2)^2 %>% sum() %>% sqrt()  # chain

# arrange
flight %>% 
	select(v1, v2) %>%
	arrange(desc(v2))
	
flight %>% arrange(data, hour, minute)

flight %>% arrange(desc(departure_delay-arrival_delay))
		
# mutate
flight <- fligth %>% 
		select(v1, v2) %>%
		mutate(v3=v1+v2, v4=v3*2)  # not necessary select variables first, can use new created variable

# summarise			
## summarise avg delay by destination
flights %>%
	group_by(Dest) %>%
	summarise(avg_delay = mean(ArrDelay, na.rm=TRUE))
	
by_date <- group_by(flights, date)
by_hour <- group_by(flights, date, hour)
by_plane <- group_by(flights, plane)  # grouping does not create a copy of the data

summarise(fliter(by_date, !is.na(dep_delay)),  # 1st filter na rows, then summarise
		med = median(dep_delay, na.rm=TRUE),
		mean = mean(dep_delay),
		max = max(dep_delay),
		q90 = quantile(dep_delay, 0.9),
		over_15 = mean(dep_delay > 15, na.rm=TRUE))

## min(x), median(x, na.rm=TRUE), max(x), quantile(x,p), sd(x), var(x), IQR(x)
## sum(x), mean(x), n(), n_distinct(x)
## sum(x>10), mean(x>10)  # logical summarry
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


