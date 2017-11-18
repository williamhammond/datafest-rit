# William Hammond

library("dplyr")


setwd('/home/william/DataFest')
dat <- read.csv('subset.csv', sep = '\t')

# Most popular channel
sum_by_channel <- dat  %>% 
  group_by(., channel) %>%
  summarise(count=sum(!is.na(user_id)))

booked <- dat[dat$is_booking == 1, ]
sum_by_channel_booked <- booked %>% 
  group_by(., channel) %>%
  summarise(count=sum(!is.na(user_id)))

sum_by_channel_booked <- rbind(sum_by_channel_booked, c(417, 0))
sum_by_channel["count_booked"] <- sum_by_channel_booked$count
sum_by_channel['ratio_success'] <- sum_by_channel$count_booked / sum_by_channel$count


abroad <- dat[(as.character(dat$user_location_country) != as.character(dat$hotel_country)), ]
abroad <- abroad[abroad$is_booking == 1, ]
