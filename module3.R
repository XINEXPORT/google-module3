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

hotel_bookings <- read_csv("hotel_bookings.csv")

head(hotel_bookings)

colnames(hotel_bookings)

str(hotel_bookings)

glimpse(hotel_bookings)

arrange(hotel_bookings, desc(lead_time))

head(hotel_bookings)

hotel_bookings_v2 <-
  arrange(hotel_bookings, desc(lead_time))

head(hotel_bookings_v2)

max(hotel_bookings$lead_time)

min(hotel_bookings$lead_time)

mean(hotel_bookings$lead_time)

####################
library(ggplot2)
library(palmerpenguins)
data(penguins)
View(penguins)

ggplot(data = penguins) + geom_point(mapping=aes(x=flipper_length_mm, y=body_mass_g))

ggplot(data=penguins, mapping=aes(x=flipper_length_mm, y=body_mass_g))+geom_point()

ggplot(data=penguins)+geom_point(mapping=aes(x=flipper_length_mm, y=body_mass_g))

hotel_bookings <- read.csv("hotel_bookings.csv")

head(hotel_bookings)
colnames(hotel_bookings)

install.packages('ggplot2')
library(ggplot2)

ggplot(data = hotel_bookings) +
  geom_point(mapping = aes(x = lead_time, y = children))

ggplot(data = hotel_bookings) +
  geom_point(mapping = aes(x = stays_in_weekend_nights, y = children))


####################
#color, size, and shape
ggplot(data=penguins)+ geom_point(mapping=aes(x=flipper_length_mm, y=body_mass_g, shape=species, color=species, size=species))

#b and white w/ transparency
ggplot(data=penguins)+ geom_point(mapping=aes(x=flipper_length_mm, y=body_mass_g, alpha=species))

#single color
ggplot(data=penguins)+ geom_point(mapping=aes(x=flipper_length_mm, y=body_mass_g),color="purple")

#smooth transition
ggplot(data=penguins)+ geom_smooth(mapping=aes(x=flipper_length_mm, y=body_mass_g),color="purple")

#geom_point + geom_smooth
ggplot(data=penguins)+ geom_point(mapping=aes(x=flipper_length_mm, y=body_mass_g)) +
  geom_smooth(mapping=aes(x=flipper_length_mm, y=body_mass_g))

#geom_smooth + jitter
ggplot(data=penguins)+ geom_smooth(mapping=aes(x=flipper_length_mm, y=body_mass_g)) +
  geom_jitter(mapping=aes(x=flipper_length_mm, y=body_mass_g))

#border color
ggplot(data=penguins)+
  geom_bar(mapping=aes(x=island,color=island))

#fill color by species
ggplot(data=penguins)+
  geom_bar(mapping=aes(x=island, fill = species))

#scatterplot for each species
ggplot(data=penguins,aes(x=flipper_length_mm, y=body_mass_g)) + 
  geom_point(aes(color=species))+
  facet_wrap(~species)

#bar chart by island and species
ggplot(data=penguins)+
  geom_bar(mapping=aes(x=species, fill=island))+
  facet_wrap(~island)

#scatter plots by sex and species
ggplot(data=penguins)+
  geom_point(mapping=aes(x=flipper_length_mm, y=body_mass_g, color=species))+
  facet_grid(sex~species)

#scatter plots by sex
ggplot(data=penguins)+
  geom_point(mapping=aes(x=flipper_length_mm, y=body_mass_g, color=species))+
  facet_grid(~sex)

head(hotel_bookings)

ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = distribution_channel))

ggplot(data = hotel_bookings) +
  geom_point(mapping = aes(x = lead_time, y = children))


ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = hotel, fill = market_segment))

ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = hotel)) +
  facet_wrap(~market_segment)


onlineta_city_hotels <- filter(hotel_bookings, 
                               (hotel=="City Hotel" & 
                                  hotel_bookings$market_segment=="Online TA"))

View(onlineta_city_hotels)


onlineta_city_hotels_v2 <- hotel_bookings %>%
  filter(hotel=="City Hotel") %>%
  filter(market_segment=="Online TA")

View(onlineta_city_hotels_v2)

ggplot(data = onlineta_city_hotels_v2) +
  geom_point(mapping = aes(x = lead_time, y = children))

##Annotation
#naming the title of a scatterplot
ggplot(data=penguins)+
  geom_point(mapping=aes(x=flipper_length_mm, y=body_mass_g, color=species))+
  labs(title="Palmer Penguins: Body vs. Flipper Length")

#include a subtitle
ggplot(data=penguins)+
  geom_point(mapping=aes(x=flipper_length_mm, y=body_mass_g, color=species))+
  labs(title="Palmer Penguins: Body vs. Flipper Length", subtitle = "Sample of 3 Penguins Species")

#add a caption
ggplot(data=penguins)+
  geom_point(mapping=aes(x=flipper_length_mm, y=body_mass_g, color=species))+
  labs(title="Palmer Penguins: Body vs. Flipper Length", subtitle = "Sample of 3 Penguins Species",
  caption = "Data collected by Dr. Kristen Gorman")

#add a label with bold, sizing, and position
ggplot(data=penguins)+
  geom_point(mapping=aes(x=flipper_length_mm, y=body_mass_g, color=species))+
  labs(title="Palmer Penguins: Body vs. Flipper Length", subtitle = "Sample of 3 Penguins Species",
       caption = "Data collected by Dr. Kristen Gorman")+
       annotate("text", x=220, y=3500, label= "The Gentoos are the largest",
           color = "purple", fontface = "bold", size = 4.5, angle = 25)

p<-ggplot(data=penguins)+
  geom_point(mapping=aes(x=flipper_length_mm, y=body_mass_g, color=species))+
  labs(title="Palmer Penguins: Body vs. Flipper Length", subtitle = "Sample of 3 Penguins Species",
       caption = "Data collected by Dr. Kristen Gorman")

#ggpplot as a varaible with annotation
p+annotate("text", x=220, y=3500, label= "The Gentoos are the largest",
           color = "blue", fontface = "bold", size = 4.5)


ggsave("Three Penguin Species.png")


####################

install.packages("rmarkdown")














