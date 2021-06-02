ui <- fluidPage( 
  titlePanel(("Interface for Interactive Plot of Family Visualization")),
  sidebarLayout(position = "left",
                sidebarPanel(id = "sidebar",
                             textInput("path", "Path for KING --ibdseg output files", defaultpath),
                             textInput("prefix", "Prefix for KING --ibdseg inference", "king"),
                             textInput("FamilyID", "Family ID for the family to be visualized", ""),
                             actionButton(inputId = "EnterFID", label = "Submit"), 
                             width = 2
                ),
                mainPanel(
                  fluidRow(
                    fluidRow(
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
