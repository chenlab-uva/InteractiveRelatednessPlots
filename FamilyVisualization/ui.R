ui <- fluidPage( 
  titlePanel(("Interface for Interactive Plot of Family Visualization")),
  sidebarLayout(position = "left",
                sidebarPanel(id = "sidebar",
                             fileInput("file1", "Choose a *splitped.txt file"),
                             fileInput("file2", "Choose a *.kin file"),
                             fluidRow(
                               column(5, offset = 0,
                                      
                                      textInput(inputId = "FamilyID",
                                                label = "Family ID",
                                                value = "", width = "100px")
                               )),
                             actionButton(inputId = "EnterFID", label = "Submit Family ID"),
                             fileInput("fileinfer", "Choose a *.seg file"),
                             fileInput("fileibdseg", "Choose a *.segments.gz file"),
                             fileInput("fileallseg", "Choose a text file with all segments information", accept = "text"),
                             
                             width = 2
                ),
                mainPanel(
                  fluidRow(
                    fluidRow("Documented versus Inferred",
                             column(6,plotOutput('plot1')),
                             column(6,plotOutput('plot2',click = "plot_click")),
                             
                             fluidRow(
                               column(width = 5,
                                      verbatimTextOutput("click_info"),
                                      verbatimTextOutput("last_infor"))
                             )
                    ),
                    fluidRow(
                      column(8, plotOutput('plot3', height="600px"))
                    )   
                    
                  )
                )
  ))
