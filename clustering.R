
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

cat$category <- as.character(cat$category)
cat$category <- ifelse(cat$category == "shops", "shopping", cat$category)
cat$category <- ifelse(cat$category == "theater", "theatre", cat$category)
cat$category <- ifelse(cat$category == "questroom", "quest", cat$category)
cat$category <- as.factor(cat$category)

# post frequency by category
cat_freq <- group_by(cat, category) %>% summarise(freq = n())

# стоит убрать категории, встречающиеся в четверти постов и более, как нерепрезентативные
# а также редкие категории
cat_todrop <- dplyr::filter(cat_freq, 
                            freq >= length(unique(kudago_match$kudago_id))/4 |
                            freq < 10)
cat <- anti_join(cat, cat_todrop, by="category")

# tags

t <- strsplit(kudago_match$tags, split = "###")
tags <- data.frame(kudago_id = rep(kudago_match$kudago_id, 
                                  sapply(t, length)), tag = unlist(t))

tags_freq <- group_by(tags, tag) %>% summarise(freq = n())

tags_todrop <- dplyr::filter(tags_freq, 
                             freq >= length(unique(kudago_match$kudago_id))/4 |
                             freq < 10)
tags <- anti_join(tags, tags_todrop, by="tag")

# category/tag -- user

cat_usr <- inner_join(cat, users_match, by = "kudago_id")
cat_usr <- group_by(cat_usr, category, users) %>% summarize(cnt_l=n())

tags_usr <- inner_join(tags, users_match, by = "kudago_id")
tags_usr <- group_by(tags_usr, tag, users) %>% summarize(cnt_l=n())

#

users_cat <- ungroup(cat_usr) %>% 
  group_by(users) %>% summarise(cnt_c=n(), sum_l=sum(cnt_l)) %>%
  inner_join(users_list, by = "users") %>% 
  dplyr::filter(cnt_c < sum_l & cnt_c > 2 & sum_l > 10)

users_tags <- ungroup(tags_usr) %>% 
  group_by(users) %>% summarise(cnt_t=n(), sum_lt=sum(cnt_l)) %>%
  inner_join(users_list, by = "users") %>% 
  dplyr::filter(cnt_t < sum_lt & cnt_t > 2)

# weights for categories

cat_freq <- group_by(cat, category) %>% summarise(freq = n())
cat_freq <- mutate(cat_freq, iprob=1-(freq/sum(cat_freq$freq)))

cat_flt <- inner_join(cat_usr, users, by = c("users"="id")) %>%
  dplyr::select(1:3) 
cat_flt <- dplyr::select(users_cat, users) %>%
  inner_join(cat_flt, by = "users")

cat_flt_freq <- group_by(cat_flt, category) %>% summarise(freq = sum(cnt_l),
                                                          uniq = n())
cat_flt <- inner_join(cat_flt, users_cat, by = "users")
cat_flt$norm <- cat_flt$cnt_l / cat_flt$sum_l

cat_pref <- inner_join(cat_flt, cat_freq, by='category') %>%
  mutate(val=norm*iprob) %>%
  dplyr::select(users,category,val) %>%
  rename(gr=category)

# weights for tags

tags_freq <- group_by(tags, tag) %>% summarise(freq = n())
tags_freq <- mutate(tags_freq, iprob=1-(freq/sum(tags_freq$freq)))

tags_flt <- inner_join(tags_usr, users, by = c("users"="id")) %>%
  dplyr::select(1:3) 
tags_flt <- dplyr::select(users_tags, users) %>%
  inner_join(tags_flt, by = "users")

tags_flt_freq <- group_by(tags_flt, tag) %>% summarise(freq = sum(cnt_l), 
                                                       uniq = n())

tags_flt <- inner_join(tags_flt, users_tags, by = "users")
tags_flt$norm <- tags_flt$cnt_l / tags_flt$sum_lt

tags_pref <- inner_join(tags_flt, tags_freq, by='tag') %>%
  mutate(val=norm*iprob) %>%
  dplyr::select(users,tag,val) %>%
  rename(gr=tag)

tags_pref <- dplyr::filter(tags_pref, users %in% unique(cat_pref$users))

#

# users_pref <- rbind(cat_pref, tags_pref)
cat_matrix <- dcast(tags_pref, users~gr, value.var = 'val')
colnames(cat_matrix) <- make.names(colnames(cat_matrix), unique = T, allow_ = T)

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
plot(res.hc0, cex = 0.01)

#

clust_8 <- cutree(res.hc0, 8)

clust8 <- as.data.frame(cbind(cat_matrix$users, clust_8))
clust8$clust_8 <- as.factor(clust8$clust_8)
colnames(clust8) <- c("users","clust")

# descriptive

summary(clust8$clust)
clust8_sum <- inner_join(clust8, users, by = c("users"="id"))

dplyr::filter(clust8_sum, clust=='1') %>% summarise(mean(sex),mean(age, na.rm = T))
dplyr::filter(clust8_sum, clust=='2') %>% summarise(mean(sex),mean(age, na.rm = T))
dplyr::filter(clust8_sum, clust=='3') %>% summarise(mean(sex),mean(age, na.rm = T))
dplyr::filter(clust8_sum, clust=='4') %>% summarise(mean(sex),mean(age, na.rm = T))
dplyr::filter(clust8_sum, clust=='5') %>% summarise(mean(sex),mean(age, na.rm = T))
dplyr::filter(clust8_sum, clust=='6') %>% summarise(mean(sex),mean(age, na.rm = T))
dplyr::filter(clust8_sum, clust=='8') %>% summarise(mean(sex),mean(age, na.rm = T))

summarise(clust8_sum, mean(sex),mean(age, na.rm = T))

# так себе отличия

clust8_cnt <- group_by(clust8, clust) %>% summarise(n=n())

clust8_cat <- inner_join(clust8, users_pref, by="users") %>%
  group_by(clust, gr) %>% summarise(avg=mean(val)) 

#

clust8_pref <- inner_join(clust8, users_match, by="users") %>%
  inner_join(tags, by='kudago_id')
clust8_pref <- clust8_pref[!duplicated(clust8_pref),]

clust_tot <- group_by(clust8_pref, tag) %>% summarise(cnt_l=n())
clust_tot_c <- group_by(clust8_pref, clust, tag) %>% summarise(cnt_c=n()) %>%
  inner_join(clust_tot, by="tag") %>%
  mutate(share=cnt_c/cnt_l)

#

dplyr::filter(clust_tot_c, clust=='1') %>% arrange(desc(share)) %>% top_n(10)
dplyr::filter(clust_tot_c, clust=='5') %>% arrange(desc(share)) %>% top_n(10)

# dplyr::filter(clust8_cat, clust=='3') %>% arrange(desc(avg)) %>% top_n(10)
# dplyr::filter(clust8_cat, clust=='6') %>% arrange(desc(avg)) %>% top_n(10)

write.csv(clust_tot_c, "~/spbRecommend/data_for_tests/clust_tot_c.csv", row.names = F)
write.csv(clust8, "~/spbRecommend/data_for_tests/clust8.csv", row.names = F)

### категории по кластерам




##### KNN?
##### (or how to assign a new member to the cluster)

library("class")
library(caret)

knn_data <- inner_join(clust8, users, by=c("users"="id")) %>% 
  inner_join(sub_mat, by=c("users"="id")) %>%
  dplyr::select(-users)
knn_data[is.na(knn_data)] = 0

write.csv(knn_data, "~/spbRecommend/data_for_tests/knn_data.csv")

set.seed(17)
test.ind = sample(seq_len(nrow(knn_data)), size = nrow(knn_data)*0.2)
test = knn_data[test.ind,]
main = knn_data[-test.ind,]

knn_result <- class::knn(train=dplyr::select(main, -clust), test=dplyr::select(test, -clust), cl=main$clust,  k=3)

confusionMatrix(knn_result,test$clust)





