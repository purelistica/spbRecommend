library(tidyverse)
library(vkR)
library(devtools)



setAccessToken(access_token='2831d46ed4624cb57f0d720f9d9396478cca91429bd1c708fc33e25fd4abad6f7e6bd637c8de6a92a67ab')

vkOAuth(5585217, 'groups', 'alina.bahitova@gmail.com', 'Qdecamz1')

y = 3718736

vk_get <- function(y){

subscriptions = data.frame()

x1 = usersGetSubscriptions(user_id = y, extended = "0", offset = 0, count = 999, flatten = T)
if (length(x1$groups$id)>1){
  subscriptions_temp = x1$groups %>% as.data.frame()
  subscriptions_temp$id = y
  subscriptions = plyr::rbind.fill(subscriptions, subscriptions_temp)} 
get_users <- function(user_ids='', fields='', name_case='') {
  code <- 'var users = [];'
  num_requests <- ifelse(length(user_ids) %% 500 == 0, (length(user_ids) %/% 500), (length(user_ids) %/% 500) + 1)
  from <- 1
  to <- ifelse(num_requests >= 2, 500, length(user_ids))
  for (i in 1:num_requests) {
    code <- paste0(code, 'users = users + API.users.get({
                   "user_ids":"', paste0(user_ids[from:to], collapse = ','), '", 
                   "fields":"', fields, '", 
                   "name_case":"', name_case, '", "v":"5.50"});')
    from <- to + 1
    to <- to + ifelse(length(user_ids) - (to + 500) >= 0, 500, length(user_ids) - to)
}
  code <- paste0(code, 'return users;')
  if (nchar(code) > 65535) stop("The POST request is limited by 65535 bytes")
  execute(code)
  }
user = data.frame()
a = get_users(user_ids = y, fields = 'sex, bdate, home_town, lists, has_mobile, contacts, site, education, followers_count, common_count, occupation, screen_name') 
a$occupationType = a$occupation$type 
a$occupationId = a$occupation$id 
a$occupationName = a$occupation$name 
if (is.null(a$occupation)==F){
  a = a %>% select( -occupation) } 
user = plyr::rbind.fill(user, a) %>% select(id:sex, bdate)
user$bdate = stringr::str_extract(user$bdate, "[0-9]{4}") %>% as.numeric()
user$age = 2017-user$bdate
user <- dplyr::select(user, -bdate)

subscriptions_matrix = data.frame(matrix(ncol = 17, nrow = 0))
x <- c('vandroukiru','sci','evil_incorparate',
       'vecherniy.urgant','just_cook',
       'modaguide','aliexpress','dayvinchik',
       'pikabu','ti_nepoverish','bez_kota',
       'ideasdecor','english_is_fun','40kg',
       's_arcazm','zenit','marvel_dc')
colnames(subscriptions_matrix) <- x
subscriptions_matrix[1,] = 0

# i = "evil_incorparate"
for (i in subscriptions$screen_name) {
  if ((i %in% x) == T) {
    io = grep(i, colnames(subscriptions_matrix))
    subscriptions_matrix[io] = 1
    
  }
}

user_final = cbind(user, subscriptions_matrix)
user_final[is.na(user_final)] = 0

return(user_final)

}
