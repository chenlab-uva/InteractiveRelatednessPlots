# ui.R
ui <- fluidPage( 
  titlePanel(("Interface for Interactive Plot of Family Visualization")),
  sidebarLayout(position = "left",
                sidebarPanel(id = "sidebar",
                             fileInput("file1", "Choose a *splitped.txt file"),
                             fileInput("file2", "Choose a *.kin file"),
                             fluidRow(
                               column(5, ofset = 3,
                                      textInput(inputId = "FamilyID",
                                                label = "FID",
                                                value = "", width = "100px")
                               )),
                             actionButton(inputId = "EnterFID", label = "Enter Sample FID"),
                             selectizeInput("Pairs_ID1_ID2", "ID1_ID2", choices =c(Choose='')),
                             #fileInput("fileibdseg", "Choose IBDSeg files", multiple=TRUE),
                             fileInput("fileinfer", "Choose a *.seg file"),
                             fileInput("fileibdseg", "Choose a *.segments.gz file"),
                             fileInput("fileallseg", "Choose a text file with all segments information", accept = "text"),
                             
                  width = 2
                ),
                mainPanel(
                  fluidRow(
                    fluidRow("Documented versus Inferred Family",
                             column(6,plotOutput('plot1')),
                             column(6,plotOutput('plot2'))
                    ),
                    fluidRow(
                             column(8, plotOutput('plot3')))                    #splitLayout(cellWidths = c("50%", "50%"),
                    #plotOutput(outputId = "plot1", width = "100%"),
                    #plotOutput(outputId = "plot2",width = "100%"),
                    #plotOutput(outputId = "plot3",width = "100%"))
                  )
  )
))
