install.packages("here")

library("here")

install.packages("skimr")

library("skimr")

install.packages("janitor")

library("janitor")

install.packages("dplyr")

library("dplyr")

install.packages("palmerpenguins")

library("palmerpenguins")

skim_without_charts(penguins)

glimpse(penguins)

head(penguins)

penguins %>%
  select (-species)

penguins %>%
  rename(island_new=island)

rename_with(penguins, tolower)

clean_names(penguins)

####################

library(tidyverse)

penguins%>% arrange(-bill_length_mm)

penguins2 <-penguins %>% arrange(-bill_length_mm)
view(penguins2)

penguins %>% group_by(island) %>% drop_na()%>% summarize(mean_bill_mm = 
                                                           mean(bill_length_mm))

penguins %>% group_by(island) %>% drop_na()%>% summarize(max_bill_mm = 
                                                           max(bill_length_mm))

penguins %>% group_by(species, island) %>% drop_na() %>% summarize(max_bl = max(bill_length_mm), mean_bl = mean(bill_length_mm))

penguins %>% filter(species =="Adelie")


####################
bookings_df <- read_csv("hotel_bookings.csv")

head(bookings_df)

str(bookings_df)

glimpse(bookings_df)

colnames(bookings_df)

skim_without_charts(bookings_df)

trimmed_df<-bookings_df%>%
  select(hotel, is_canceled, lead_time )

trimmed_df
glimpse(trimmed_df)
colnames(trimmed_df)

trimmed_df %>% 
  select(hotel, is_canceled, lead_time) %>% 
  rename(hotel_type = hotel)

example_df <- bookings_df %>%
  select(arrival_date_year, arrival_date_month) %>% 
  unite(arrival_month_year, c("arrival_date_month", "arrival_date_year"), sep = " ")

example_df <- bookings_df %>%
  mutate(guests = adults + children + babies )

example_df <- bookings_df %>% summarize(number_canceled = sum(is_canceled),
                                        average_lead_time = mean(lead_time))
  
head(example_df)
glimpse(example_df)

####################

  
  






