
# display data
flights <- tbl_df(hflights)  # convert to loca ldata frame for efficient display

print(flight, n=20)

data.frame(head(flights))  # convert to a normal data frame to see all of vars

glimpse(mammals)  # look at data in a transpose view

# filter rows
filter(flights, Month==1, DayofMonth==1)  # M==1& DayofM==1
filter(flights, Month==1|Month==2)  # M==1 or 2
filter(flights, Month %in% c(1,2))  # M==1 or 2
filter(flights, hour >= 0, hour <= 5)  # common represent "And"
filter(flights, delay_time >= 2*delay_min)  # common represent "And"
filter(flights, !is.na(dep_delay))  # filter na rows

# select cols
select(flight, v1, v2, v3)
select(flight, v1:v3)
select(flight, v1:v10, contains("Taxi"), contains("Deylay"))
select(flight, ends_with("delay"))
# starts_with, ends_with, contains, matches, num_range, one_of("x","y","z"), everything()
select(flight, v_new=v1, v_old=v2, v3) # using select to rename variables

#Select seems to work with the column indexes
Vars <- c("v1", "v2","v3")
d <- d %>% filter(year %in% c(2017)) %>% select(match(Vars, names(.))) 


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
# aggregation function: n inputs -> 1 output
## summarise avg delay by destination
flights %>%
	group_by(Dest) %>%  # group_by does not create a copy of data and change the order of the data
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
# ctrl+shift+p in RStudio rerun previous submitted code

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
	
# window function: take n inputs -> n outputs
# 1.ranking and ordering:
# min_rank(c(1,1,2,3) [1] 1 1 3 4  # ordinary ranking
# dense_rank(c(1,1,2,3) [1] 1 1 2 4 # no duplicate count for tie number in ranking
# row_number(c(1,1,2,3) [1] 1 2 3 4 # ignore tie and give ranking
# 2. offsets: lead & lag
# 3. cumulative aggregates
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

# do()
df %>% group_by(ID) %>% do(na.locf(.))

df %>% group_by(ID) %>% do(data.frame(year=.$year[1])

df %>% group_by(ID) %>% do(head(.,2))

models <- dat %>% group_by(date) %>% 
	do(
		mod = lm(dep_delay ~ time, data=.)
	)


# plot by group
library(dplyr)
library(ggplot2)
library(scales) 

setClass("toDate")
setAs("character", "toDate", function(from) as.Date(from, format="%m/%d/%Y"))

d <- read.csv("Bukom Thickness PSI data_FS.csv", header=TRUE, colClasses=c("Sent.Time"="toDate"))
d <- d[, c(1,3,4)]
names(d) <- c("ID", "Time", "UTMeasurement")

by_id <- group_by(d, ID)  # group data by ID

stat <- summarise(filter(by_id, !is.na(UTMeasurement)),  # summary statistics
                    n = n(),
                    min = min(UTMeasurement),
                    max = max(UTMeasurement),
                    mean = mean(UTMeasurement),
                    median = median(UTMeasurement),
                    sd = sd(UTMeasurement),
                    p80 = quantile(UTMeasurement, 0.8),
                    p85 = quantile(UTMeasurement, 0.85),
                    p90 = quantile(UTMeasurement, 0.9),
                    p95 = quantile(UTMeasurement, 0.95)
                  )

by_id %>% filter(!is.na(UTMeasurement)) %>%  
          do({ 
          	p <- ggplot(data=., aes(x=Time, y=UTMeasurement)) + geom_point() + scale_x_date(labels = date_format("%m/%d/%y")) + ggtitle(unique(.$ID)) 
        	ggsave(p, filename=paste0(unique(.$ID),".png"))
            })


