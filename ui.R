library(markdown)


fluidPage(style="padding-top: 80px;",
          #h1("Absolutely-positioned panels"),
          wellPanel(
            #bottom = 20, right = 20, width = 300,
            #draggable = TRUE,
            style="width:30%; float:right; font-size: 20px;",
              HTML(markdownToHTML(fragment.only=TRUE, text=c(
                "Please be patient and tell us a little bit about `yourself`:"
              ))),
              sliderInput("age", "Age", min=14, max=65, value=20),
              selectInput("sex", "Sex", c("Male", "Female")),
              textInput("subway", "Subway station", value = "Маяковская")
              ),
            
          absolutePanel(
            top = 0, left = 0, right = 0,
            fixed = TRUE,
            div(
              style="padding: 20px; font-size: 32px;text-align: center; border-bottom: 1px solid #CCC; background: #FFFFEE;",
              HTML(markdownToHTML(fragment.only=TRUE, text=c(
                "WELCOME TO KUDAGO RECOMMENDATIONS PAGE"
              )))
            )
          ),
          wellPanel(
            img(src='http://az616578.vo.msecnd.net/files/2016/03/23/635943566212276867210249251_concert-crowd.jpg', align = "right"
                ,width="220", height="155"),
            style="width:70%;height:180px; font-size: 20px;",
            HTML(markdownToHTML(fragment.only=TRUE, text=c(toString(textOutput("plot22")))
            )),div(
              HTML(markdownToHTML(fragment.only=TRUE, text=c(
                toString(textOutput("desc2"))
              ))),style="font-size: 14px;",actionButton("plot2","Узнать больше")
            )
          ),
          wellPanel(
            img(src='http://az616578.vo.msecnd.net/files/2016/03/23/635943566212276867210249251_concert-crowd.jpg', align = "right"
                ,width="220", height="155"),
            style="width:70%;height:180px; font-size: 20px;",
            HTML(markdownToHTML(fragment.only=TRUE, text=c(toString(textOutput("plot2")))
              )),div(
              HTML(markdownToHTML(fragment.only=TRUE, text=c(
                toString(textOutput("desc"))
              ))),style="font-size: 14px;",actionButton("plot2","Узнать больше")
            )
          ),
          wellPanel(
            img(src='http://az616578.vo.msecnd.net/files/2016/03/23/635943566212276867210249251_concert-crowd.jpg', align = "right"
                ,width="220", height="155"),
            style="width:70%;height:180px; font-size: 20px;",
            HTML(markdownToHTML(fragment.only=TRUE, text=c(toString(textOutput("plot23")))
            )),div(
              HTML(markdownToHTML(fragment.only=TRUE, text=c(
                toString(textOutput("desc3"))
              ))),style="font-size: 14px;",actionButton("plot2","Узнать больше")
            )
          ),
          wellPanel(
            img(src='http://az616578.vo.msecnd.net/files/2016/03/23/635943566212276867210249251_concert-crowd.jpg', align = "right"
                ,width="220", height="155"),
            style="width:70%;height:180px; font-size: 20px;",
            HTML(markdownToHTML(fragment.only=TRUE, text=c(toString(textOutput("plot4")))
            )),div(
              HTML(markdownToHTML(fragment.only=TRUE, text=c(
                toString(textOutput("desc4"))
              ))),style="font-size: 14px;",actionButton("plot2","Узнать больше")
            )
          ),
          wellPanel(
            img(src='http://az616578.vo.msecnd.net/files/2016/03/23/635943566212276867210249251_concert-crowd.jpg', align = "right"
                ,width="220", height="155"),
            style="width:70%;height:180px; font-size: 20px;",
            HTML(markdownToHTML(fragment.only=TRUE, text=c(toString(textOutput("plot5")))
            )),div(
              HTML(markdownToHTML(fragment.only=TRUE, text=c(
                toString(textOutput("desc5"))
              ))),style="font-size: 14px;",actionButton("plot2","Узнать больше")
            )
          ),
          wellPanel(
            img(src='http://az616578.vo.msecnd.net/files/2016/03/23/635943566212276867210249251_concert-crowd.jpg', align = "right"
                ,width="220", height="155"),
            style="width:70%;height:180px; font-size: 20px;",
            HTML(markdownToHTML(fragment.only=TRUE, text=c(toString(textOutput("plot6")))
            )),div(
              HTML(markdownToHTML(fragment.only=TRUE, text=c(
                toString(textOutput("desc6"))
              ))),style="font-size: 14px;",actionButton("plot2","Узнать больше")
            )
          )
          
            )
              
