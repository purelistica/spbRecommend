
### workfile

library(readr)
likes <- read_csv("/students/aabakhitova/spbRecommend/likes_vk2000.csv")
likes <- likes[,2:3]
posts <- read_csv("/students/aabakhitova/spbRecommend/vk_posts_light.csv")
posts <- posts[,1:19]
events <- read_csv('events.csv')
places <- read_csv('places.csv')

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

library(dplyr)
# posts <- dplyr::select(posts, -(10:19))
posts <- dplyr::select(posts, id, link, comments.count, likes.count, reposts.count)

colnames(events)[1] <- "kudago_id"
events <- dplyr::select(events, kudago_id, title, body_text, 
                        description, site_url, categories, tags)
events$site_url <- trim(events$site_url)

colnames(places)[1] <- "kudago_id"
places <- dplyr::select(places, kudago_id, title, body_text, 
                        description, site_url, categories, tags)
places$site_url <- trim(places$site_url)

all <- rbind(events,places)

s <- strsplit(all$categories, split = "###")
cat <- data.frame(kudago_id = rep(all$kudago_id, sapply(s, length)), category = unlist(s))
cat <- inner_join(cat, match, by = "kudago_id")
cat <- group_by(cat, category, users) %>% summarize(cnt=n())

library(reshape2)

res <- dcast(cat, users~category, value.var = 'cnt')

###

test <- inner_join(all, posts, by=c("site_url"="link"))
test_sum2 <- dplyr::group_by(test2, site_url) %>% summarise(cnt=n()) %>% arrange(desc(cnt))

###

match <- inner_join(test, likes, by="id")
match <- dplyr::select(match, kudago_id, users)

###







