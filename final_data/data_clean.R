
# data preprocessing file

library(readr)
library(dplyr)
library(lubridate)

# data loading

# likes <- read_csv("/students/aabakhitova/spb_files/likes_vk10000.csv")
posts <- read_csv("/students/aabakhitova/spb_files/vk_posts_light2.csv")

events <- read_csv('~/spbRecommend/kudago_data/events2.csv')
events_dates <- read_csv('~/spbRecommend/kudago_data/events2_dates.csv')
places <- read_csv('~/spbRecommend/kudago_data/places2.csv')

# vk data cleaning

posts <- posts[,1:19]

## link transform
posts$link <- paste(posts$attachments.link.url1,
                    posts$attachments.link.url2,
                    posts$attachments.link.url3,
                    posts$attachments.link.url4,
                    posts$attachments.link.url5,
                    posts$attachments.link.url6,
                    posts$attachments.link.url7,
                    posts$attachments.link.url8,
                    posts$attachments.link.url9,
                    posts$attachments.link.url10)
posts$link <- gsub("NA", "", posts$link)
posts$link <- tolower(posts$link)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
posts$link <- trim(posts$link)

## 

posts <- dplyr::filter(posts, post_type == "post")
posts <- dplyr::select(posts, id, date, link, 
                       comments.count, likes.count, reposts.count)
posts <- dplyr::filter(posts, link != "")
posts$date <- as.POSIXct(posts$date, origin="1970-01-01")

# today

today <- as.Date(max(posts$date))

# kudago data clean&transform

## filter past events

events_dates$start <- as.POSIXct(events_dates$start, origin="1970-01-01")
events_dates$end <- as.POSIXct(events_dates$end, origin="1970-01-01")
events_dates <- dplyr::filter(events_dates, start >= today)

events_dates_act <- unique(events_dates$id)
events$act <- ifelse(events$id %in% events_dates_act, 1, 0)

## mix places & events

colnames(events)[1] <- "kudago_id"
events <- dplyr::select(events, kudago_id, title, body_text, 
                        description, site_url, categories, tags,
                        age_restriction, price, is_free,
                        image, act)
events$site_url <- trim(events$site_url)
events$type <- "event"

colnames(places)[1] <- "kudago_id"
places$act <- 1
places <- dplyr::select(places, kudago_id, title, body_text, 
                        description, site_url, categories, tags,
                        image, act)
places$site_url <- trim(places$site_url)
places$type <- "place"
places$age_restriction <- NA
places$price <- NA
places$is_free <- NA

# full kudago dataset

kudago_all <- rbind(events,places)

# match of kudago & vk

kudago_match <- inner_join(kudago_all, posts, by=c("site_url"="link"))
write.csv(kudago_match, "~/spbRecommend/final_data/kudago_match.csv")

