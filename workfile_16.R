

sub <- read_csv('/students/aabakhitova/spb_files/users_subscriptions.csv')
sub <- sub[,1:3]
sub <- dplyr::select(fin_mat, users) %>% inner_join(sub, by=c("users"="id"))

sub_uniq <- group_by(sub, screen_name) %>% summarise(cnt=n())
sub_uniq <- dplyr::filter(sub_uniq, cnt > 100 & cnt < length(unique(sub$users))/4)

sub <- inner_join(sub, sub_uniq, by='screen_name')


sub_uniq <- group_by(sub, screen_name) %>% summarise(cnt=n())
hist(sub_uniq$cnt, xlim=c(50,150), breaks=5000)
sub_uniq <- dplyr::filter(sub_uniq, cnt > 100 & cnt < length(unique(sub$id))/4)

# sub_clust <- inner_join(sub, clust8, by=c("id"="users")) %>% 
#   group_by(clust, screen_name) %>% summarise(cnt=n())

# dplyr::filter(sub_clust, clust == '1') %>% arrange(desc(cnt)) %>% top_n(10)
# dplyr::filter(sub_clust, clust == '3') %>% arrange(desc(cnt)) %>% top_n(10)

sub_test <- dplyr::filter(sub, screen_name %in% c('vandroukiru','sci','evil_incorparate',
                                                  'vecherniy.urgant','just_cook',
                                                  'modaguide','aliexpress','dayvinchik',
                                                  'pikabu','ti_nepoverish','bez_kota',
                                                  'ideasdecor','english_is_fun','40kg',
                                                  's_arcazm','zenit','marvel_dc'))
sub_test <- sub_test[!duplicated(sub_test),]
sub_test$val <- 1

sub_mat <- dcast(sub_test, id~screen_name, value.var = "val")
sub_mat[is.na(sub_mat)] = 0

# ----------------------------------------------------------------------------

library(igraph)

sub1 <- sub[1:500000,]
sub2 <- sub[500001:1000000,]
sub3 <- sub[1000001:1500000,]
sub4 <- sub[1500001:2000000,]
sub5 <- sub[2000001:2333119,]

users <- data.frame(name = unique(sub5$users), type=T)
pub <- data.frame(name = unique(sub5$screen_name), type=F)
nodes <- rbind(users, pub)
edges = data.frame(from=sub5$users, to=sub5$screen_name)

g <- graph_from_data_frame(edges, directed=F, vertices=nodes)

pr = bipartite.projection(g)
p5 <- pr[[1]]

a5 <- get.edgelist(p5, names=TRUE)
a5 <- as.data.frame(a5)
a5$weight <- E(p5)$weight

a <- rbind(a1,a2)
a <- group_by(a,V1,V2) %>% summarise(w=sum(weight))
aa <- rbind(a3,a4,a5)
aa <- group_by(aa,V1,V2) %>% summarise(w=sum(weight))

aaa <- rbind(a,aa)
aaa$V1 <- as.factor(aaa$V1)
aaa <- group_by(aaa,V1,V2) %>% summarise(w1=sum(w))
aaa2 <- dplyr::filter(aaa2, w1 > 30)

# hist(aaa2$w1, breaks=1000, xlim=c(0,100))

# -----
# links graph

aaa_nodes <- data.frame(name = unique(aaa2$V1)) %>% rbind(data.frame(name = unique(aaa2$V2)))
aaa_nodes <- aaa_nodes[!duplicated(aaa_nodes),]

aaa_gr <- graph_from_data_frame(aaa2, directed=T, vertices=aaa_nodes)
E(aaa_gr)$weight <- aaa2$w1

aaa_gr2 <- as.undirected(aaa_gr, mode="collapse")
aaa_gr2 <- simplify(aaa_gr2, remove.multiple = T, remove.loops = T)

# write.csv(aaa2, "~/spbRecommend/data_for_tests/aaa2.csv")

# fc <- fastgreedy.community(aaa_gr2)

fc2 <- cluster_fast_greedy(aaa_gr2, merges = TRUE, modularity = TRUE,
                           membership = TRUE, weights = E(aaa_gr2)$weight)

mmb <- membership(fc2)
mmb <- data.frame(as.numeric(mmb))
mmb$name <- V(aaa_gr2)$name
colnames(mmb) <- c('com','screen_name')
mmb <- dplyr::filter(mmb, com %in% c(1,2))

write.csv(mmb, '~/spbRecommend/data_for_tests/mmb.csv')

# ---------------------------------------------

# select subs

# sub_user <- dplyr::select(sub, users, screen_name)
# sub_user <- sub_user[!duplicated(sub_user),]
# sub_user$val <- 1
# write.csv(sub_user,"~/spbRecommend/data_for_tests/sub_user.csv", row.names = F)

sub_user <- read_csv("~/spbRecommend/data_for_tests/sub_user.csv")

sub_uniq <- group_by(sub_user, screen_name) %>% summarise(n())
j <- sub_uniq[1:3,]

sub_user <- inner_join(sub_user, j, by="screen_name") 

sub_mat <- dcast(sub_user, users~screen_name, value.var = "val")
colnames(sub_mat) <- make.names(colnames(sub_mat), unique = T, allow_ = T)

knn_data <- inner_join(knn_data, sub_mat, by="users") %>% dplyr::select(-users, -X1)

knn_data$clust <- as.factor(knn_data$clust)
knn_data[is.na(knn_data)] = 0
set.seed(17)
test.ind = sample(seq_len(nrow(knn_data)), size = nrow(knn_data)*0.3)
test = knn_data[test.ind,]
main = knn_data[-test.ind,]

library(randomForest)
rf_model <- randomForest(clust ~ ., data=main)








