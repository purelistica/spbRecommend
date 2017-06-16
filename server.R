function(input, output, session) {
  

    model <- eventReactive( c(input$age, input$sex), {
      # draw a random number and print it
      input$age
      data = read.csv("/students/sekozlov/HOMEWORK/project2/events_s.csv")
      p = sample(1:8000, 1)
      p1 = as.character(data$title[p])
      i1 = as.character(data$image[p])
      download.file(i1,basename("i1.jpg"))
      p2 = gsub('"' , "",substring(toString(as.character(data$description[p])),4))
      p = sample(1:8000, 1)
      p3 = as.character(data$title[p])
      i3 = as.character(data$image[p])
      download.file(i3,basename("i3.jpg"))
      p4 = gsub('"' , "",substring(toString(as.character(data$description[p])),4))
      p = sample(1:8000, 1)
      p5 = as.character(data$title[p])
      i5 = as.character(data$image[p])
      p6 = gsub('"' , "",substring(toString(as.character(data$description[p])),4))
      p = sample(1:8000, 1)
      p7 = as.character(data$title[p])
      i7 = as.character(data$image[p])
      p8 = gsub('"' , "",substring(toString(as.character(data$description[p])),4))
      p = sample(1:8000, 1)
      p9 = as.character(data$title[p])
      i9 = as.character(data$image[p])
      p10 = gsub('"' , "",substring(toString(as.character(data$description[p])),4))
      p = sample(1:8000, 1)
      p11 = as.character(data$title[p])
      i11 = as.character(data$image[p])
      p12 = gsub('"' , "",substring(toString(as.character(data$description[p])),4))
      # return all object as a list
      return(list(p2 = p2, p1=p1,p3 = p3, p4=p4,p5 = p5, p6=p6,p7 = p7, p8=p8,p9 = p9, p10=p10,
                  p11 = p11, p12=p12, i1=i1, i3=i3, i5=i5, i7=i7, i9=i9, i11=i11))
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
      img(src=model()$i1, align = "right"
          ,width="220", height="155")
      
    })
    output$img3 <- renderUI({
      img(src=model()$i3, align = "right"
          ,width="220", height="155")
      
    })
    output$img5 <- renderUI({
      img(src=model()$i5, align = "right"
          ,width="220", height="155")
      
    })
    output$img7 <- renderUI({
      img(src=model()$i7, align = "right"
          ,width="220", height="155")
      
    })
    output$img9 <- renderUI({
      img(src=model()$i9, align = "right"
          ,width="220", height="155")
      
    })
    output$img11 <- renderUI({
      img(src=model()$i11, align = "right"
          ,width="220", height="155")
      
    })
}
    
    
