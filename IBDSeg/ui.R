ui <- fluidPage(
  titlePanel(("Interface for Interactive Plot of IBD Segments")),
  sidebarLayout(position = "left",
                sidebarPanel(id = "sidebar",
                             fileInput("fileinfer", "Choose a *.seg file"),
                             fileInput("fileibdseg", "Choose a *.segments.gz file"),
                             fileInput("fileallseg", "Choose a text file with all segments information", accept = "text"),
                             sliderInput("IBD1Seg", "IBD1Seg_Range:", min = 0, max = 1,value = c(0,1)),
                             sliderInput("IBD2Seg", "IBD2Seg_Range:",min = 0, max = 1,value = c(0,1)),
                             width = 2
                ),
                mainPanel(id="main",
                          tabPanel("Plots", 
                                   fluidRow(
                                     splitLayout(style = "border: 1px solid silver:", 
                                                 plotOutput(outputId = "plot1", click = "plot_click", height = "600px"),
                                                 plotOutput(outputId = "plot2", height = "600px", width = "100%")
                                     )),
                                   fluidRow(
                                     column(width = 5,
                                            verbatimTextOutput("click_info"),
                                            verbatimTextOutput("last_infor")        
                                     )
                                   )
                          )
                )))

