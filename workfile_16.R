

sub <- read_csv('/students/aabakhitova/spb_files/users_subscriptions.csv')
sub <- sub[,1:3]

sub_uniq <- group_by(sub, screen_name) %>% summarise(cnt=n())
sub_uniq <- dplyr::filter(sub_uniq, cnt > 70)

sub_clust <- inner_join(sub, clust8, by=c("id"="users")) %>% 
  group_by(clust, screen_name) %>% summarise(cnt=n())

# dplyr::filter(sub_clust, clust == '1') %>% arrange(desc(cnt)) %>% top_n(10)
# dplyr::filter(sub_clust, clust == '3') %>% arrange(desc(cnt)) %>% top_n(10)

sub_test <- dplyr::filter(sub, screen_name %in% c('vandroukiru','sci','evil_incorparate',
                                                  'vecherniy.urgant','just_cook',
                                                  'modaguide','aliexpress'))
sub_test <- sub_test[!duplicated(sub_test),]
sub_test$val <- 1

sub_mat <- dcast(sub_test, id~screen_name, value.var = "val")
sub_mat[is.na(sub_mat)] = 0

# ----------------------------------------------------------------------------

library(igraph)

sub_sh <- sub[1:1000,]

users <- data.frame(name = unique(sub_sh$id), type=T)
pub <- data.frame(name = unique(sub_sh$screen_name), type=F)
nodes <- rbind(users, pub)
edges = data.frame(from=sub_sh$id, to=sub_sh$screen_name)

g <- graph_from_data_frame(edges, directed=TRUE, vertices=nodes)

pr = bipartite.projection(g)
p <- pr[[1]]




