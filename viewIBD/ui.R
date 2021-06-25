ui <- fluidPage(
  titlePanel(("Interface for Interactive Plot of Identical-By-Descent Segments")),
  sidebarLayout(position = "left",
                sidebarPanel(id = "sidebar", strong("Please prepare KING ibdseg output files and then"),
                             actionButton(inputId = "filechoose", label = "Choose *.seg file"),
                             fluidRow(
                               column(8,
                                      textInput(inputId = "FID",
                                                label = "Family ID (optional)",
                                                value = "All")
                               )),
                             actionButton(inputId = "AllFID", label = "Select all samples"),
                             actionButton(inputId = "EnterFID", label = "Generate interactive plots"),
                             sliderInput("IBD1Seg", "IBD1Seg_Range:", min = 0, max = 1,value = c(0,1)),
                             sliderInput("IBD2Seg", "IBD2Seg_Range:", min = 0, max = 1,value = c(0,1)),
                             conditionalPanel(
                               condition = "input.FID!= 'All' && input.FID.length >0 ",
                               #condition = "input.FID!= null",
                               selectizeInput("IDs", "All inferred relatives",choices =c(Choose=''))
                               
                             ),
                             width = 2
                ),
                mainPanel(
                  tabsetPanel(id = "inTabset", selected = "panel1",
                    tabPanel("Main Plot", value = "panel1",
                             fluidRow(
                               splitLayout(style = "border: 1px solid silver:", 
                                           plotOutput(outputId = "plot1", click = "plot_click", height = "600px"),
                                           plotOutput(outputId = "plot2", height = "600px", width = "100%")
                               )),
                             fluidRow(
                               dataTableOutput(outputId = "dt1")
                             )
                    ),
                    tabPanel("IBD Segments for the Selected Pair", value = "panel2",
                             plotOutput("plot3",height = "600px", width = "80%"),
                             dataTableOutput(outputId = "dt2")
                    )
                    
                  )))
)
 
