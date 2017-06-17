
######################## RECOMMENDATION SYSTEM ########################

load("~/spbRecommend/recSys_demo/test_1714.RData")
source('~/spbRecommend/r_shiny.R')

y = 3718736
x <- vk_get(y)[,4:22]

test_pred <- predict(knn_fit, newdata = x)
result <- test_pred[1]

# генерируем рекоммендацию для данного пользователя

opt <- dplyr::filter(clust_tot_c, clust == result) %>% arrange(desc(share)) %>% 
  top_n(5) %>% inner_join(kudago_rec_tags, by='tag') %>%
  inner_join(kudago_rec, by='kudago_id') %>%
  dplyr::select(kudago_id, title, description, site_url, image)

ind <- sample(seq_len(nrow(opt)), size = 8)

rec <- opt[ind,]
rec
