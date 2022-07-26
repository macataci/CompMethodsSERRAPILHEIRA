library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
covid <- read.csv("data/raw/covid19.csv")
# First checking the class
class(covid$date)

# Changing to date format
covid$date <- as_date(covid$date)

# Checking the class
class(covid$date)

# Now we can make numeric operations
range(covid$date)

ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  theme_minimal()

#covid$new_confirmed[covid$new_confirmed < 0] <- 0
covid$new_confirmed[covid$new_confirmed < 0] <- abs(covid$new_confirmed)
covid$new_confirmed <- ifelse(covid$new_confirmed < 0,
                              abs(covid$new_confirmed),
                              covid$new_confirmed)

ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  theme_minimal() +
  scale_x_date(breaks="4 months", date_labels="%Y-%m")+
  labs(x = "Date", y = "New cases")


covid$roll_mean <- zoo::rollmean(covid$new_confirmed, 14, fill=NA)
head(covid)
ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  theme_minimal() +
  scale_x_date(breaks="4 months", date_labels="%Y-%m")+
  labs(x = "Date", y = "New cases")+
  geom_line(aes(x = date, y = roll_mean), color="red", size=1.2)

