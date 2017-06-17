
# RECOMMENDER (kind of)

library(readr)
library(dplyr)

clust8 <- read_csv("~/spbRecommend/data_for_tests/clust8.csv")
clust_tot_c <- read_csv("~/spbRecommend/data_for_tests/clust_tot_c.csv")
knn_data <- read_csv("~/spbRecommend/data_for_tests/knn_data.csv")

# data

# kudago_rec <- dplyr::filter(kudago_all, act == 1)
# kudago_rec$description1 <- gsub("<[^>]+>", "", kudago_rec$description)
# kudago_rec$description <- gsub("<.*?>", "", kudago_rec$description)
# write.csv(kudago_rec,"~/spbRecommend/data_for_tests/kudago_rec.csv", row.names = F)

kudago_rec <- read_csv("~/spbRecommend/data_for_tests/kudago_rec.csv")

t2 <- strsplit(kudago_rec$tags, split = "###")
kudago_rec_tags <- data.frame(kudago_id = rep(kudago_rec$kudago_id, 
                                   sapply(t2, length)), tag = unlist(t2))
 

# предполагаем, что пользователь относится к определенному кластеру

library("class")
library(caret)

knn_data$clust <- as.factor(knn_data$clust)
# set.seed(17)
# test.ind = sample(seq_len(nrow(knn_data)), size = nrow(knn_data)*0.2)
# test = knn_data[test.ind,]
# main = knn_data[-test.ind,]

knn_fit <- train(clust ~., data = knn_data, method = "knn", tuneLength=5)


# test_pred <- predict(knn_fit, newdata = test)
# confusionMatrix(test_pred, test$clust)

source('~/spbRecommend/r_shiny.R')
y = 3718736
x <- vk_get(y)[,4:22]

test_pred <- predict(knn_fit, newdata = x)
test_pred[1]

# result <- 5
result <- test_pred[1]

# генерируем рекоммендацию для данного пользователя

opt <- dplyr::filter(clust_tot_c, clust == result) %>% arrange(desc(share)) %>% 
  top_n(5) %>% inner_join(kudago_rec_tags, by='tag') %>%
  inner_join(kudago_rec, by='kudago_id') %>%
  dplyr::select(kudago_id, title, description, site_url, image)

ind <- sample(seq_len(nrow(opt)), size = 8)

rec <- opt[ind,]
rec
