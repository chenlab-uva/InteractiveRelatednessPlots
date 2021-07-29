ui <- fluidPage(
  titlePanel(("Interface for Interactive Plot of Individual-Level Relatedness")),
  sidebarLayout(position = "left",
                sidebarPanel(id = "sidebar",
                             strong("Step 1: Please prepare KING ibdseg output files and then"),
                             actionButton(inputId = "filechoose", label = "Choose *.seg file"),
                             htmlOutput("text"),
                             h5(""),
                             textInput(inputId = "FID",label = "Optional Step 2: Please type a family ID, click the button, or skip this step", value = "All"),
                             actionButton(inputId = "AllFID", label = "Select all samples"), 
                             h1(""),
                             h5(strong("Additional options")),
                             sliderInput("IBD1Seg", "IBD1Seg_Range:", min = 0, max = 1,value = c(0,1)),
                             sliderInput("IBD2Seg", "IBD2Seg_Range:", min = 0, max = 1,value = c(0,1)),
                             conditionalPanel(
                               condition = "input.FID!= 'All' && input.FID.length >0 ",
                               selectizeInput("IDs", "Optional Step 3: Please select from the following list of all inferred relatives", choices =c(Choose=''))
                               
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
                              tabPanel("Plot for Optional Step 3", value = "panel2",
                                       plotOutput("plot3",height = "600px", width = "80%"),
                                       dataTableOutput(outputId = "dt2")
                              )
                              
                  )))
)