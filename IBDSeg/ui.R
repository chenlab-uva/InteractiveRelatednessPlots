ui <- fluidPage(
  titlePanel(("Interface for Interactive Plot of Identical-By-Descent Segments")),
  sidebarLayout(position = "left",
                sidebarPanel(id = "sidebar",
                             fileInput("fileinfer", "Choose a *.seg file"),
                             fileInput("fileibdseg", "Choose a *.segments.gz file"),
                             fileInput("fileallseg", "Choose a text file with all segments information", accept = "text"),
                             sliderInput("IBD1Seg", "IBD1Seg_Range:", min = 0, max = 1,value = c(0,1)),
                             sliderInput("IBD2Seg", "IBD2Seg_Range:", min = 0, max = 1,value = c(0,1)),
                             fluidRow(
                               column(5, textInput(inputId = "ID1",
                                                   label = "ID1",
                                                   value = " ",
                                                   width = "100px")
                               ),
                               column(5, ofset = 3,
                                      textInput(inputId = "ID2",
                                                label = "ID2",
                                                value = "",
                                                width = "100px")
                               )),
                             actionButton(inputId = "EnterIDs", label = "Enter Sample1 ID and Sample2 ID"),
                             width = 2
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("Main Plot", 
                             fluidRow(
                               splitLayout(style = "border: 1px solid silver:", 
                                           plotOutput(outputId = "plot1", click = "plot_click", height = "600px"),
                                           plotOutput(outputId = "plot2", height = "600px", width = "100%")
                               )),
                             fluidRow(
                               column(width = 8,
                                      verbatimTextOutput("click_info"),
                                      verbatimTextOutput("last_infor")        
                               )
                             )
                    ),
                    tabPanel("IBD Segments for the Selected Pair",
                             plotOutput("plot3",height = "600px", width = "80%"),
                             dataTableOutput(outputId = "dt1")
                    )
                    
                  )))
)
