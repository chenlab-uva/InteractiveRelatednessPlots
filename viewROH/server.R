server <- function(input, output, session) {
  
  path <- reactiveValues(
    pth=NULL
  )
  
  prefix <- reactiveValues(
    name = NULL
  )
  
  
  observeEvent(input$filechoose,{
    fullpath <- file.choose()
    file.base <- basename(fullpath)
    file.dir <- dirname(fullpath)
    file.prefix <- gsub(".roh","", file.base)
    prefix$name <- file.prefix
    path$pth <- paste(file.dir, file.prefix, sep = "/")
    updateTextInput(session, inputId = "FID", label = paste("Optional Step 2: Please type a family ID in", file.prefix, "data, click the button, or skip this step"), value = "All")
    output$text <- renderText({
      paste(file.base,"is loaded", "<br>", paste0(file.prefix, ".rohseg.gz"), "is loaded", "<br>", paste0(file.prefix, "allsegs.txt"), "is loaded.")
    })
    updateTabsetPanel(session, "inTabset", selected = "panel1")
  })
  
  observeEvent(input$SelectAll,{
    req(path$pth)
    req(prefix$name)
    updateTabsetPanel(session, "inTabset", selected = "panel1")
    updateTextInput(session, inputId = "FID", label = paste("Optional Step 2: Please type a family ID in", prefix$name, "data, click the button, or skip this step"), value = "All")
  })
  
  
  observeEvent(input$ID, {
    updateTabsetPanel(session, "inTabset", selected = "panel2")
  })
  
  
  roh_info_df <- reactive({
    req(path$pth)
    req(input$FID)
    withProgress(message = 'Loading .roh file',
                 detail = 'This may take a while...', value = 0, {
                   incProgress(1/1)
                   roh <- read.table(paste0(path$pth,".roh"), header = TRUE, stringsAsFactors = FALSE)
                 })
    if (input$FID != "All")  roh <- roh[roh$FID == input$FID, ]
    shiny::validate(
      need(nrow(roh) > 0, "please type a valid Family ID")
    )
    if ("F_ROH_X" %in% colnames(roh)) {
      rohinfo <- roh[roh$F_ROH > 2^-6.5, c("FID","ID","F_ROH_X","F_ROH")]
    } else {
      set.seed(123)
      roh$F_ROH_X <- round(runif(nrow(roh), 0,1),4)
      roh$tmp_F_ROH_X <- 1
      rohinfo <- roh[roh$F_ROH > 2^-6.5, c("FID","ID","F_ROH_X","F_ROH", "tmp_F_ROH_X")]
    }
    shiny::validate(
      need(nrow(rohinfo) > 0, "No samples with F_ROH larger than 2^-6.5")
    )
    updateSliderInput(session, "F_ROH_X_Range",label = "F_ROH_X_Range",
                      min = round(min(rohinfo$F_ROH_X),4), max = round(max(rohinfo$F_ROH_X),4), 
                      value = c(
                        round(min(rohinfo$F_ROH_X),4), round(max(rohinfo$F_ROH_X),4))
    )
    updateSliderInput(session, "F_ROH_Range",label = "F_ROH_Range",
                      min = round(min(rohinfo$F_ROH),4), max = round(max(rohinfo$F_ROH),4), 
                      value = c(
                        round(min(rohinfo$F_ROH),4),
                        round(max(rohinfo$F_ROH),4)
                      )
    )
    if (input$FID!= "All") {
      updateSelectizeInput(session, "ID", label = paste("Optional Step 3: Please select from the following list of samples in family", input$FID),
                           choices = c(Choose='', rohinfo$ID), selected = NULL)
    } 
    return(rohinfo)
  })
  
  segments_df <- reactive({
    req(path$pth)
    shiny::validate(
      need(file.exists(paste0(path$pth, ".rohseg.gz")), paste0(path$pth, ".rohseg.gz is missing"))
    )
    
    withProgress(message = 'Loading .rohseg.gz file',
                 detail = 'This may take a while...', value = 0, {
                   incProgress(1/1)
                   rohseg <- read.table(paste0(path$pth, ".rohseg.gz"), header = TRUE, stringsAsFactors = FALSE)
                 })
    rohseg <- rohseg[, c("FID", "ID", "Chr", "StartMB", "StopMB")]
    return(rohseg)
  })
  
  all_seg_df <- reactive({
    req(path$pth)
    shiny::validate(
      need(file.exists(paste0(path$pth, "allsegs.txt")), paste0(path$pth, "allsegs.txt is missing"))
    )
    withProgress(message = 'Loading allsegs.txt file',
                 detail = 'This may take a while...', value = 0, {
                   incProgress(1/1)
                   allseg <- read.table(paste0(path$pth, "allsegs.txt"), header = TRUE)
                 })
    
    allseg <- allseg[, c("Chr", "StartMB","StopMB")]
    return(allseg)
  })
  
  output$plot1 <- renderPlot({
    req(roh_info_df())
    req(segments_df())
    req(all_seg_df())
    prefix <- prefix$name
    roh_info <- roh_info_df()
    target.data <- roh_info[roh_info$F_ROH >= input$F_ROH_Range[1] & roh_info$F_ROH <= input$F_ROH_Range[2] & 
                              roh_info$F_ROH_X >= input$F_ROH_X_Range[1] & roh_info$F_ROH_X <= input$F_ROH_X_Range[2],]
    shiny::validate(
      need(nrow(target.data) > 0, "No samples in this region. Please adjust F_ROH and F_ROH_X")
    )
    ylab.title <- ifelse("tmp_F_ROH_X" %in% colnames(target.data), "F_ROH_X (Randomly Generated)",
                         "F_ROH_X")
    plot(target.data$F_ROH, target.data$F_ROH_X, xlab = "F_ROH", ylab= ylab.title, main = paste0("Interactive Display of ROH in ", prefix, " with Clickable Dots"),
         cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
  })
  
  output$plot2 <- renderPlot({
    req(input$plot_click)
    roh_info <- roh_info_df()
    segments <- segments_df()
    all_seg <- all_seg_df()
    min.index <- which.min(abs(roh_info$F_ROH_X-input$plot_click$y)^2 + abs(roh_info$F_ROH-input$plot_click$x)^2)
    nameID <- roh_info[min.index,"ID"]
    nameFID <- roh_info[min.index,"FID"]
    if (!(abs(roh_info[min.index,"F_ROH"]-input$plot_click$x) <= 0.01 & abs(roh_info[min.index,"F_ROH_X"]-input$plot_click$y) <= 0.01)) {
      k <- NULL
    } else {
      k <- segments[segments$FID==nameFID & segments$ID==nameID, ]
    }
    shiny::validate(
      need(nrow(k) > 0, "Please select a sample")
    )
    theme_set(theme_bw(base_size = 16))
    f_roh <- roh_info[roh_info$FID==nameFID & roh_info$ID==nameID,"F_ROH"]
    fid <- k[1,1]
    id <- k[1,2]
    prefix <- prefix$name
    g <- ggplot() +
      geom_rect(data = all_seg, aes(xmin = StartMB, xmax = StopMB, ymin = 0, max = 0.9), fill = 'white', color = "black", size = 0.85) +
      geom_rect(data = k, aes(xmin = StartMB, xmax = StopMB, ymin = 0, ymax = 0.9), fill = "red") +
      geom_rect(data = all_seg, aes(xmin = StartMB, xmax = StopMB, ymin = 0, max = 0.9), color = "black", alpha = 0, size = 0.85) +
      facet_grid(Chr ~ .) + scale_x_continuous(expand  = c(0, 0), limits = c(0, NA)) +
      labs(x = "Position (Mb)", y = "", title = bquote(paste('ROH for ', .(id), ' from FAM ', .(fid), ' in ', .(prefix), ' (F'['ROH']*' = ', .(f_roh), ')'))) +
      theme(legend.position = "none",
            panel.background = element_rect(fill = 'grey80', color = 'grey80'), panel.border = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.title=element_text(size = 14))
    print(g)
  })
  
  
  output$dt1 <- renderDataTable({
    req(roh_info_df())
    req(segments_df())
    req(all_seg_df())
    req(input$plot_click)
    roh_info <- roh_info_df()
    segments <- segments_df()
    all_seg <- all_seg_df()
    min.index <- which.min(abs(roh_info$F_ROH_X-input$plot_click$y)^2 + abs(roh_info$F_ROH-input$plot_click$x)^2)
    nameID <- roh_info[min.index,"ID"]
    nameFID <- roh_info[min.index,"FID"]
    if (!(abs(roh_info[min.index,"F_ROH"]-input$plot_click$x) <= 0.01 & abs(roh_info[min.index,"F_ROH_X"]-input$plot_click$y) <= 0.01)) {
      k <- NULL
    } else {
      k <- segments[segments$FID==nameFID & segments$ID==nameID, ]
    }
    shiny::validate(
      need(nrow(k) > 0, "Please select a sample")
    )
    k
  })
  
  
  
  output$plot3 <- renderPlot({
    req(input$ID)
    roh_info <- roh_info_df()
    all_seg <- all_seg_df()
    prefix <- prefix$name
    allrohgz <- segments_df()
    k <- allrohgz[allrohgz$ID == input$ID, ]
    fid <- k$FID[1]
    id <- k$ID[1]
    f_roh <- roh_info[roh_info$FID==fid & roh_info$ID==id,"F_ROH"]
    
    theme_set(theme_bw(base_size = 16))
    g <- ggplot() +
      geom_rect(data = all_seg, aes(xmin = StartMB, xmax = StopMB, ymin = 0, max = 0.9), fill = 'white', color = "black", size = 0.85) +
      geom_rect(data = k, aes(xmin = StartMB, xmax = StopMB, ymin = 0, ymax = 0.9), fill = "red") +
      geom_rect(data = all_seg, aes(xmin = StartMB, xmax = StopMB, ymin = 0, max = 0.9), color = "black", alpha = 0, size = 0.85) +
      facet_grid(Chr ~ .) + scale_x_continuous(expand  = c(0, 0), limits = c(0, NA)) +
      labs(x = "Position (Mb)", y = "", title = bquote(paste('ROH ', .(id), ' from FAM ', .(fid), ' in ', .(prefix), ' (F'['ROH']*' = ', .(f_roh), ')'))) +
      theme(legend.position = "none",
            panel.background = element_rect(fill = 'grey80', color = 'grey80'), panel.border = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.title=element_text(size = 15))
    print(g)
    
  })
  
  output$dt2 <- renderDataTable({
    req(input$ID)
    allrohgz <- segments_df()
    select_df  <- allrohgz[allrohgz$ID == input$ID, ]
    shiny::validate(need(nrow(select_df) > 0, "Please select a sample in the study dataset"))
    select_df
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
}

