ui <- fluidPage( 
  titlePanel(("Interface for Interactive Plot of Family Relatedness")),
  sidebarLayout(position = "left", 
                sidebarPanel(id = "sidebar", strong("Step 1: Please prepare KING ibdseg output files and then"),
                             actionButton(inputId = "filechoose", label = "Choose *.seg file"),
                             textOutput("text"),
                             h5(""),
                             textInput(inputId = "FamilyIDtype", label = "Step 2: Please type an ID for the family to be visualized", value = NULL),
                             selectizeInput("FamilySize", "Or Step 2b: Please specify the family size", choices =c(Choose='')),
                             conditionalPanel(
                               condition = "input.FamilySize > 0",
                               selectizeInput("FamilyID", "And then choose a family", 
                                              choices =c(Choose=''))
                             ),
                             width = 2
                ),
                mainPanel(
                  tabsetPanel(id = "inTabset",
                              tabPanel("Plot for Step 2", value = "panel1",
                                       fluidRow(
                                         column(6,plotOutput('plot1')),
                                         column(6,plotOutput('plot2',click = "plot_click_main"))
                                       ),
                                       fluidRow(
                                         column(12, plotOutput('plot3', height="600px")),
                                         column(12, dataTableOutput(outputId = "dt1"))
                                       )
                                       
                                       
                              ),
                              tabPanel("Plot for Step 2b", value = "panel2",
                                       fluidRow(
                                         column(6,plotOutput('plot4')),
                                         column(6,plotOutput('plot5',click = "plot_click"))
                                       ),
                                       fluidRow(
                                         column(12, plotOutput('plot6', height="600px")),
                                         column(12, dataTableOutput(outputId = "dt2"))
                                       )  
                              )
                              
                  ))
  ))