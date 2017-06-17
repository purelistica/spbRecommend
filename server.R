function(input, output, session) {


    model <- eventReactive( c(input$sub_id), {
      # draw a random number and print it
      input$sub_id
      data = read.csv("/students/sekozlov/HOMEWORK/project2/events_s.csv")
      p = sample(1:8000, 1)
      p1 = as.character(data$title[p])
      i1 = as.character(data$image[p])
      u1 = as.character(data$site_url[p])
      p2 = gsub('"' , "",substring(toString(as.character(data$description[p])),4))
      p = sample(1:8000, 1)
      p3 = as.character(data$title[p])
      i3 = as.character(data$image[p])
      u3 = as.character(data$site_url[p])
      p4 = gsub('"' , "",substring(toString(as.character(data$description[p])),4))
      p = sample(1:8000, 1)
      p5 = as.character(data$title[p])
      i5 = as.character(data$image[p])
      u5 = as.character(data$site_url[p])
      p6 = gsub('"' , "",substring(toString(as.character(data$description[p])),4))
      p = sample(1:8000, 1)
      p7 = as.character(data$title[p])
      i7 = as.character(data$image[p])
      u7 = as.character(data$site_url[p])
      p8 = gsub('"' , "",substring(toString(as.character(data$description[p])),4))
      p = sample(1:8000, 1)
      p9 = as.character(data$title[p])
      i9 = as.character(data$image[p])
      u9 = as.character(data$site_url[p])
      p10 = gsub('"' , "",substring(toString(as.character(data$description[p])),4))
      p = sample(1:8000, 1)
      p11 = as.character(data$title[p])
      i11 = as.character(data$image[p])
      u11 = as.character(data$site_url[p])
      p12 = gsub('"' , "",substring(toString(as.character(data$description[p])),4))
      library(tidyverse)
      library(vkR)
      library(devtools)
      setAccessToken(access_token='2831d46ed4624cb57f0d720f9d9396478cca91429bd1c708fc33e25fd4abad6f7e6bd637c8de6a92a67ab')
      
      vkOAuth(5585217, 'groups', 'alina.bahitova@gmail.com', 'Qdecamz1')
      
      y = input$subway
      
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
      user$bdate = 2017-user$bdate
      
      subscriptions_matrix = data.frame(matrix(ncol = 7, nrow = 0))
      x <- c('vandroukiru','sci','evil_incorparate',
             'vecherniy.urgant','just_cook',
             'modaguide','aliexpress')
      colnames(subscriptions_matrix) <- x
      subscriptions_matrix[1,] = 0
      
      i = "evil_incorparate"
      for (i in subscriptions$screen_name) {
        if ((i %in% x) == T) {
          io = grep(i, colnames(subscriptions_matrix))
          subscriptions_matrix[io] = 1
          
        }
      }
      
      user_final = cbind(user, subscriptions_matrix)
      user_final[is.na(user_final)] = 0
      username = paste(user$first_name, user$last_name, sep = " ")
      # return all object as a list
      return(list(p2 = p2, p1=p1,p3 = p3, p4=p4,p5 = p5, p6=p6,p7 = p7, p8=p8,p9 = p9, p10=p10,
                  p11 = p11, p12=p12, i1=i1, i3=i3, i5=i5, i7=i7, i9=i9, i11=i11, username=username,
                  u1=u1,u3=u3,u5=u5,u7=u7,u9=u9,u11=u11))
    })
     output$plot2 <- renderText({
      model()$p1
    })
    output$desc <- renderText({
      model()$p2
    })
    

    output$plot22 <- renderText({
      model()$p3
    })
    output$desc2 <- renderText({
      model()$p4
    })
      output$plot23 <- renderText({
      model()$p5
    })
    output$desc3 <- renderText({
      model()$p6
    })
    
    output$plot4 <- renderText({
      model()$p7
    })
    output$desc4 <- renderText({
      model()$p8
    })
    
    output$plot5 <- renderText({
      model()$p9
    })
    output$desc5 <- renderText({
      model()$p10
    })
    output$plot6 <- renderText({
      model()$p11
    })
    output$desc6 <- renderText({
      model()$p12
    })
    output$img1 <- renderUI({
      tags$a(img(src=model()$i1, align = "right"
          ,width="220", height="155"),href=model()$u1,target="_blank")
      
    })
    output$img3 <- renderUI({
      tags$a(img(src=model()$i3, align = "right"
                 ,width="220", height="155"),href=model()$u3,target="_blank")
      
    })
    output$img5 <- renderUI({
      tags$a(img(src=model()$i5, align = "right"
                 ,width="220", height="155"),href=model()$u5,target="_blank")
      
    })
    output$img7 <- renderUI({
      tags$a(img(src=model()$i7, align = "right"
                 ,width="220", height="155"),href=model()$u7,target="_blank")
      
    })
    output$img9 <- renderUI({
      tags$a(img(src=model()$i9, align = "right"
                 ,width="220", height="155"),href=model()$u9,target="_blank")
      
    })
    output$img11 <- renderUI({
      tags$a(img(src=model()$i11, align = "right"
                 ,width="220", height="155"),href=model()$u11,target="_blank")
      
    })
    
    output$but1 <- renderUI({
    actionButton(inputId='ab1', label="Для перехода нажмите на изображение", 
                 icon = icon("eye"))
    })
    
    output$username <- renderText({
      model()$username
    })
}
    
    
