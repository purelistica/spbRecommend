
# users clustering and data processing

library(readr)
library(stringr)
library(reshape2)
library(dplyr)

users <- read_csv("~/spbRecommend/vk/users_get.csv")

# users prep 

users$year <- str_split_fixed(users$bdate, "\\.", 3)[,3]
users$year <- ifelse(users$year == "", NA, users$year)
users$year <- as.integer(users$year)
users$age <- 2017 - users$year
users <- dplyr::select(users, id, sex, age)

# categories

s <- strsplit(kudago_match$categories, split = "###")
cat <- data.frame(kudago_id = rep(kudago_match$kudago_id, 
                                  sapply(s, length)), category = unlist(s))
cat <- inner_join(cat, users_match, by = "kudago_id")
cat <- group_by(cat, category, users) %>% summarize(cnt=n())

cat$category <- as.character(cat$category)
cat$category <- ifelse(cat$category == "shops", "shopping", cat$category)
cat$category <- ifelse(cat$category == "theater", "theatre", cat$category)
cat$category <- ifelse(cat$category == "questroom", "quest", cat$category)
cat$category <- as.factor(cat$category)

cat <- dplyr::filter(cat, !(category %in% c('exhibition','tour','sport')))
cat <- dplyr::group_by(cat, category, users) %>% summarise(cnt_l=sum(cnt))

# frequency by category

cat_freq <- group_by(cat, category) %>% summarise(freq = sum(cnt_l))
# to filter or not to filter ... ? 

users_cat <- ungroup(cat) %>% 
  group_by(users) %>% summarise(cnt_c=n(), sum_l=sum(cnt_l)) %>%
  inner_join(users_list, by = "users")

users_cat_flt <- dplyr::filter(users_cat, cnt_c < sum_l & cnt_c > 2 &
                                 sum_l > 10)

cat_flt <- inner_join(cat, users, by = c("users"="id")) %>%
  dplyr::select(1:3) 
cat_flt <- dplyr::select(users_cat_flt, users) %>%
  inner_join(cat_flt, by = "users")
# наверное, стоит нормировать, чтобы у более редких был вес больше

cat_flt_freq <- group_by(cat_flt, category) %>% summarise(freq = sum(cnt_l),
                                                          uniq = n())
cat_flt_freq <- dplyr::filter(cat_flt_freq, freq > uniq, uniq > 100)

cat_flt <- inner_join(cat_flt, cat_flt_freq, by = "category") %>% 
  dplyr::select(1:3)

# число в матрице -- доля лайков пользователя в данной категории

cat_flt <- inner_join(cat_flt, users_cat, by = "users")
cat_flt$norm <- cat_flt$cnt_l / cat_flt$sum_l

cat_matrix <- dcast(cat_flt, users~category, value.var = 'norm')

###

fin_mat <- inner_join(cat_matrix, users, by = c("users"="id")) %>% dplyr::select(-age)
fin_mat$sex <- fin_mat$sex-1
#fin_mat$age <- fin_mat$age/10

fin_mat[is.na(fin_mat)] = 0
dist_mat = dist(fin_mat)

# CLUSTERING

res.hc0 <- hclust(dist_mat, method = "ward.D2")
#res.hc1 <- hclust(dist_mat, method = "average")
res.hc2 <- hclust(dist_mat, method = "complete")
#res.hc3 <- hclust(dist_mat, method = "single")
plot(res.hc3, cex = 0.1)

#

clust_7 <- cutree(res.hc0, 7)

clust7 <- as.data.frame(cbind(cat_matrix$users, clust_7))
clust7$clust_7 <- as.factor(clust7$clust_7)
colnames(clust7) <- c("users","clust")

# descriptive

summary(clust7$clust)
clust7_sum <- inner_join(clust7, users, by = c("users"="id"))

dplyr::filter(clust7_sum, clust=='1') %>% summarise(mean(sex),mean(age, na.rm = T))
dplyr::filter(clust7_sum, clust=='2') %>% summarise(mean(sex),mean(age, na.rm = T))
dplyr::filter(clust7_sum, clust=='3') %>% summarise(mean(sex),mean(age, na.rm = T))
dplyr::filter(clust7_sum, clust=='4') %>% summarise(mean(sex),mean(age, na.rm = T))
dplyr::filter(clust7_sum, clust=='5') %>% summarise(mean(sex),mean(age, na.rm = T))
dplyr::filter(clust7_sum, clust=='6') %>% summarise(mean(sex),mean(age, na.rm = T))
dplyr::filter(clust7_sum, clust=='7') %>% summarise(mean(sex),mean(age, na.rm = T))

summarise(clust7_sum, mean(sex),mean(age, na.rm = T))

# так себе отличия

clust7_cat <- inner_join(clust7, cat_flt, by="users") %>%
  group_by(clust, category) %>% summarise(avg=mean(norm))

dplyr::filter(clust7_cat, clust=='1') %>% arrange(desc(avg)) %>% top_n(10)
dplyr::filter(clust7_cat, clust=='2') %>% arrange(desc(avg)) %>% top_n(10)
dplyr::filter(clust7_cat, clust=='3') %>% arrange(desc(avg)) %>% top_n(10)
dplyr::filter(clust7_cat, clust=='6') %>% arrange(desc(avg)) %>% top_n(10)

