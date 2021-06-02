ui <- fluidPage(
  titlePanel(("Interface for Interactive Plot of Run-Of-Homozygosity")),
  sidebarLayout(position = "left",
                sidebarPanel(id = "sidebar",
                             fileInput("fileroh", "Choose a roh file"),
                             fileInput("filerohseg", "Choose a rohseg.gz file"),
                             fileInput("fileallseg", "Choose a text file with all segments information", accept = "text"),
                             textInput(inputId = "FID", label = "Family ID", value = "All"),
                             actionButton(inputId = "EnterFID", label = "Generate Plots"),
                             #actionButton(inputId = "ClearFID", label = "Clear Family ID"),
                             sliderInput("F_ROH_Range", "F_ROH_Range:", min = 0, max = 1,value = c(0,1)),
                             sliderInput("F_ROH_X_Range", "F_ROH_X_Range:",min = 0, max = 1,value = c(0,1)),
                             selectizeInput("ID", "Sample ID",choices =c(Choose='')),
                             width = 2
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
                               dataTableOutput(outputId = "dt1")
                             ),
                             fluidRow(
                               column(width = 5,
                                      verbatimTextOutput("click_info"),
                                      verbatimTextOutput("last_infor"))
                             )
                    ),
                    
                    tabPanel("ROH for Selected Study Sample",
                             plotOutput("plot3",height = "600px", width = "80%"),
                             dataTableOutput(outputId = "dt2")
                    )
                  ))
  ))
