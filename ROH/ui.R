ui <- fluidPage(
  titlePanel(("Interface for Interactive Plot of Run-Of-Homozygosity")),
  sidebarLayout(position = "left",
                sidebarPanel(id = "sidebar",
                             fileInput("fileroh", "Choose a roh file"),
                             fileInput("filerohseg", "Choose a rohseg.gz file"),
                             fileInput("fileallseg", "Choose a text file with all segments information", accept = "text"),
                             sliderInput("F_ROH_range", "F_ROH_Range:", min = 0, max = 1,value = c(0,1)),
                             sliderInput("F_ROH_X_range", "F_ROH_X_Range:",min = 0, max = 1,value = c(0,1)),
                             selectizeInput("ID", "ID",choices =c(Choose='')),
                             width = 3
                ),
                 mainPanel(
                  tabsetPanel(
                    tabPanel("Main Plot", 
                             fluidRow(
                               splitLayout(style = "border: 1px solid silver:", 
                                           plotOutput(outputId = "plot1", click = "plot_click",height = "600px"),
                                           plotOutput(outputId = "plot2", height = "600px", width = "100%")
                               )),
                             fluidRow(
                               column(width = 5,
                                      verbatimTextOutput("click_info"),
                                      verbatimTextOutput("last_infor"))
                             )
                    ),
                    
                    tabPanel("ROH for Selected Study Sample",
                             plotOutput("plot3",height = "600px", width = "80%"),
                             dataTableOutput(outputId = "dt1")
                             )
                                              ))
))
