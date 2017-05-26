function(input, output, session) {
  

    model <- eventReactive(input$age, {
      # draw a random number and print it
      input$age
      data = read.csv("/students/sekozlov/HOMEWORK/project2/events_s.csv")
      data$description=str_replace(data$description, "</p>", "")
      p = sample(1:8000, 1)
      p1 = as.character(data$title[p])
      p2 = gsub('"' , "",substring(toString(as.character(data$description[p])),4))
      
      # return all object as a list
      return(list(p2 = p2, p1=p1))
    })
     output$plot2 <- renderText({
      model()$p1
    })
    output$desc <- renderText({
      model()$p2
    })
    model1 <- eventReactive(input$age, {
      # draw a random number and print it
      input$age
      data = read.csv("/students/sekozlov/HOMEWORK/project2/events_s.csv")
      data$description=str_replace(data$description, "</p>", "")
      p = sample(1:8000, 1)
      p1 = as.character(data$title[p])
      p2 = gsub('"' , "",substring(toString(as.character(data$description[p])),4))
      
      # return all object as a list
      return(list(p2 = p2, p1=p1))
    })
    output$plot22 <- renderText({
      model1()$p1
    })
    output$desc2 <- renderText({
      model1()$p2
    })
    model2 <- eventReactive(input$age, {
      # draw a random number and print it
      input$age
      data = read.csv("/students/sekozlov/HOMEWORK/project2/events_s.csv")
      data$description=str_replace(data$description, "</p>", "")
      p = sample(1:8000, 1)
      p1 = as.character(data$title[p])
      p2 = gsub('"' , "",substring(toString(as.character(data$description[p])),4))
      
      # return all object as a list
      return(list(p2 = p2, p1=p1))
    })
    output$plot23 <- renderText({
      model2()$p1
    })
    output$desc3 <- renderText({
      model2()$p2
    })
    model4 <- eventReactive(input$age, {
      # draw a random number and print it
      input$age
      data = read.csv("/students/sekozlov/HOMEWORK/project2/events_s.csv")
      data$description=str_replace(data$description, "</p>", "")
      p = sample(1:8000, 1)
      p1 = as.character(data$title[p])
      p2 = gsub('"' , "",substring(toString(as.character(data$description[p])),4))
      
      # return all object as a list
      return(list(p2 = p2, p1=p1))
    })
    output$plot4 <- renderText({
      model4()$p1
    })
    output$desc4 <- renderText({
      model4()$p2
    })
    model5 <- eventReactive(input$age, {
      # draw a random number and print it
      input$age
      data = read.csv("/students/sekozlov/HOMEWORK/project2/events_s.csv")
      data$description=str_replace(data$description, "</p>", "")
      p = sample(1:8000, 1)
      p1 = as.character(data$title[p])
      p2 = gsub('"' , "",substring(toString(as.character(data$description[p])),4))
      
      # return all object as a list
      return(list(p2 = p2, p1=p1))
    })
    output$plot5 <- renderText({
      model5()$p1
    })
    output$desc5 <- renderText({
      model5()$p2
    })
    model6 <- eventReactive(input$age, {
      # draw a random number and print it
      input$age
      data = read.csv("/students/sekozlov/HOMEWORK/project2/events_s.csv")
      data$description=str_replace(data$description, "</p>", "")
      p = sample(1:8000, 1)
      p1 = as.character(data$title[p])
      p2 = gsub('"' , "",substring(toString(as.character(data$description[p])),4))
      
      # return all object as a list
      return(list(p2 = p2, p1=p1))
    })
    output$plot6 <- renderText({
      model6()$p1
    })
    output$desc6 <- renderText({
      model6()$p2
    })    
    output$img <- renderImage({
      ret_img <- function(age){
        if (age > 25) {
          return(list(
            src = "http://az616578.vo.msecnd.net/files/2016/03/23/635943566212276867210249251_concert-crowd.jpg",
            #contentType = "image/jpg",
            align = "right"
            ,width="220", height="155"
          ))
        }
        else {
          return(list(
            src = "https://media.livenationinternational.com/lincsmedia/Media/z/x/n/5a8f31cf-c577-b1e6-3536-1b2846806108.jpg",
            #contentType = "image/jpg",
            align = "right"
            ,width="220", height="155"
          ))
        }
      }
      p2 = ret_img(input$age)
      return(p2)
    })
  }
