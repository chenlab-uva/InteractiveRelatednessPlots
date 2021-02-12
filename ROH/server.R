server <- function(input, output, session) {
  
  roh_info_df <- reactive({
    req(input$fileroh)
    fileroh <- input$fileroh
    roh <- read.table(fileroh$datapath, header = TRUE, stringsAsFactors = FALSE)
    rohinfo <- roh[roh$F_ROH > 2^-6.5, c("FID","ID","F_ROH_X","F_ROH")]
    updateSelectizeInput(session, "ID", label = "ID", choices = c(Choose='', rohinfo$ID), selected = NULL)
    updateSliderInput(session, "F_ROH_Range",label = "F_ROH_Range",
                      min = min(rohinfo$F_ROH), max = max(rohinfo$F_ROH), 
                      value = c(min(rohinfo$F_ROH),max(rohinfo$F_ROH)))
    return(rohinfo)
  })
  
  segments_df <- reactive({
    req(input$filerohseg)
    filerohseg <- input$filerohseg
    rohseg <- read.table(filerohseg$datapath, header = TRUE, stringsAsFactors = FALSE)
    rohseg <- rohseg[, c("FID", "ID", "Chr", "StartMB", "StopMB")]
    return(rohseg)
  })
  
  all_seg_df <- reactive({
    req(input$fileallseg)
    fileallseg <- input$fileallseg
    allseg <- read.table(fileallseg$datapath, header = TRUE)
    allseg <- allseg[, c("Chr", "StartMB","StopMB")]
    return(allseg)
  })
  
  filename <- reactive({
    req(input$fileroh)
    file.info <- input$fileroh
    file_prefix <- gsub(".roh", " ", file.info$name)
    return(file_prefix)
  })
  
  input_target <- eventReactive(input$EnterID, {
    req(input$filerohseg)
    filerohseg <- input$filerohseg
    rohseg <- read.table(filerohseg$datapath, header = TRUE, stringsAsFactors = FALSE)
    rohseg <- rohseg[, c("FID", "ID", "Chr", "StartMB", "StopMB")]
    select_rohseg <- rohseg[(rohseg$FID==input$FID & rohseg$ID==input$ID), ]
    return(select_rohseg)
  })
  
  
  
  output$plot1 <- renderPlot({
    req(roh_info_df())
    prefix <- filename()
    roh_info <- roh_info_df()
    target.data <- roh_info[roh_info$F_ROH >= input$F_ROH_Range[1] & roh_info$F_ROH <= input$F_ROH_Range[2] & 
                              roh_info$F_ROH_X >= input$F_ROH_X_Range[1] & roh_info$F_ROH_X <= input$F_ROH_X_Range[2],]
    validate(
      need(nrow(target.data) > 0, "No samples in this region. Please adjust the F_ROH and F_ROH_X")
    )
    plot(target.data$F_ROH, target.data$F_ROH_X, xlab = "F_ROH", ylab="F_ROH_X", main = paste0("F_ROH_X vs F_ROH in ", prefix),
         cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
  })
  
  output$click_info <- renderPrint({
    req(roh_info_df())
    req(segments_df())
    req(all_seg_df())
    if(!is.null(input$plot_click)){
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
        write.table(k, row.names = FALSE, quote = FALSE, sep = "\t")
      }
      output$plot2 <- renderPlot({
        validate(
          need(nrow(k) > 0, "Please select a sample in the study dataset")
        )
        theme_set(theme_bw(base_size = 16))
        f_roh <- roh_info[roh_info$FID==nameFID & roh_info$ID==nameID,"F_ROH"]
        fid <- k[1,1]
        id <- k[1,2]
        prefix <- filename()
        g <- ggplot() +
          geom_rect(data = all_seg, aes(xmin = StartMB, xmax = StopMB, ymin = 0, max = 0.9), fill = 'white', color = "black", size = 0.85) +
          geom_rect(data = k, aes(xmin = StartMB, xmax = StopMB, ymin = 0, ymax = 0.9), fill = "red") +
          geom_rect(data = all_seg, aes(xmin = StartMB, xmax = StopMB, ymin = 0, max = 0.9), color = "black", alpha = 0, size = 0.85) +
          facet_grid(Chr ~ .) + scale_x_continuous(expand  = c(0, 0), limits = c(0, NA)) +
          labs(x = "Position (Mb)", y = "", title = bquote(paste('Run of Homozygosity for ', .(id), ' from FAM ', .(fid), ' in ', .(prefix), ' (F'['ROH']*' = ', .(f_roh), ')'))) +
          theme(legend.position = "none",
                panel.background = element_rect(fill = 'grey80', color = 'grey80'), panel.border = element_blank(),
                panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.title=element_text(size = 15))
        print(g)
        
      })
    }
  })
  
  output$plot3 <- renderPlot({
    req(input$EnterID)
    req(roh_info_df())
    req(input_target())
    req(all_seg_df())
    
    roh_info <- roh_info_df()
    all_seg <- all_seg_df()
    
    prefix <- filename()
    
    k <- input_target()
    validate(
      need(nrow(k) > 0, "Please select a sample in the study dataset")
    )
    fid <- k$FID[1]
    id <- k$ID[1]
    f_roh <- roh_info[roh_info$FID==fid & roh_info$ID==id,"F_ROH"]
    
    theme_set(theme_bw(base_size = 16))
    g <- ggplot() +
      geom_rect(data = all_seg, aes(xmin = StartMB, xmax = StopMB, ymin = 0, max = 0.9), fill = 'white', color = "black", size = 0.85) +
      geom_rect(data = k, aes(xmin = StartMB, xmax = StopMB, ymin = 0, ymax = 0.9), fill = "red") +
      geom_rect(data = all_seg, aes(xmin = StartMB, xmax = StopMB, ymin = 0, max = 0.9), color = "black", alpha = 0, size = 0.85) +
      facet_grid(Chr ~ .) + scale_x_continuous(expand  = c(0, 0), limits = c(0, NA)) +
      labs(x = "Position (Mb)", y = "", title = bquote(paste('Run of Homozygosity for ', .(id), ' from FAM ', .(fid), ' in ', .(prefix), ' (F'['ROH']*' = ', .(f_roh), ')'))) +
      theme(legend.position = "none",
            panel.background = element_rect(fill = 'grey80', color = 'grey80'), panel.border = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.title=element_text(size = 15))
    print(g)
    
  })
  
  output$dt1 <- renderDataTable({
    req(input$EnterID)
    select_dt1 <- input_target()
    validate(
      need(nrow(select_dt1) > 0, "Please select a sample in the study dataset")
    )
    select_dt1
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
}
