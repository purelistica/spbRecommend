load("/students/avsmirnova_4/spbRecommend/0526_2254.RData")
res.hc <- hclust(dist_res, method = "ward.D2" )

fviz_nbclust(dist_res, hcut, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)
