function(input, output, session) {
  

    model <- eventReactive( c(input$age, input$sex), {
      # draw a random number and print it
      input$age
      data = read.csv("/students/sekozlov/HOMEWORK/project2/events_s.csv")
      p = sample(1:8000, 1)
      p1 = as.character(data$title[p])
      p2 = gsub('"' , "",substring(toString(as.character(data$description[p])),4))
      p = sample(1:8000, 1)
      p3 = as.character(data$title[p])
      p4 = gsub('"' , "",substring(toString(as.character(data$description[p])),4))
      p = sample(1:8000, 1)
      p5 = as.character(data$title[p])
      p6 = gsub('"' , "",substring(toString(as.character(data$description[p])),4))
      p = sample(1:8000, 1)
      p7 = as.character(data$title[p])
      p8 = gsub('"' , "",substring(toString(as.character(data$description[p])),4))
      p = sample(1:8000, 1)
      p9 = as.character(data$title[p])
      p10 = gsub('"' , "",substring(toString(as.character(data$description[p])),4))
      p = sample(1:8000, 1)
      p11 = as.character(data$title[p])
      p12 = gsub('"' , "",substring(toString(as.character(data$description[p])),4))
      # return all object as a list
      return(list(p2 = p2, p1=p1,p3 = p3, p4=p4,p5 = p5, p6=p6,p7 = p7, p8=p8,p9 = p9, p10=p10,
                  p11 = p11, p12=p12))
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
