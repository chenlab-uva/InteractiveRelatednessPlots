ui <- fluidPage(
  titlePanel(("Interface for Interactive Plot of Run-Of-Homozygosity")),
  sidebarLayout(position = "left",
                sidebarPanel(id = "sidebar", strong("Step 1: Please prepare KING roh output files and then"),
                             actionButton(inputId = "filechoose", label = "Choose *.roh file"),
                             htmlOutput('text'),
                             h5(""),
                             textInput(inputId = "FID", label = "Optional Step 2: Please type a family ID, click the button, or skip this step", value = "All"),
                             actionButton(inputId = "SelectAll", label = "Select all samples"),
                             h5(""),
                             h5(strong("Additional options")),
                             sliderInput("F_ROH_Range", "F_ROH_Range:", min = 0, max = 1,value = c(0,1)),
                             sliderInput("F_ROH_X_Range", "F_ROH_X_Range:",min = 0, max = 1,value = c(0,1)),
                             conditionalPanel(
                               condition = "input.FID!= 'All' && input.FID.length >0 ",
                               selectizeInput("ID", "Optional Step 3: Please select from the following list of samples",choices =c(Choose=''))
                             ),
                             width = 2
                ),
                mainPanel(
                  tabsetPanel(id = "inTabset", selected = "panel1",
                              tabPanel(title = "Main Plot", value = "panel1",
                                       fluidRow(
                                         splitLayout(style = "border: 1px solid silver:", 
                                                     plotOutput(outputId = "plot1", click = "plot_click",height = "600px"),
                                                     plotOutput(outputId = "plot2", height = "600px", width = "100%")
                                         )),
                                       fluidRow(
                                         dataTableOutput(outputId = "dt1")
                                       )
                              ),
                              tabPanel(title = "Plot for Optional Step 3", value = "panel2",
                                       plotOutput("plot3",height = "600px", width = "80%"),
                                       dataTableOutput(outputId = "dt2")
                              )
                  ))
  ))