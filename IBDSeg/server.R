server <- function(input, output, session) {
  
  infer_df <- reactive({
    req(input$fileinfer)
    fileinfer <- input$fileinfer
    infer_all <- read.table(fileinfer$datapath, header = TRUE, stringsAsFactors = FALSE)
    updateSliderInput(session, "IBD1Seg",label = "IBD1Seg",
                      min = min(infer_all$IBD1Seg), max = max(infer_all$IBD1Seg), 
                      value = c(min(infer_all$IBD1Seg),max(infer_all$IBD1Seg)))
    updateSliderInput(session, "IBD2Seg",label = "IBD2Seg",
                      min = min(infer_all$IBD2Seg), max = max(infer_all$IBD2Seg), 
                      value = c(min(infer_all$IBD2Seg),max(infer_all$IBD2Seg)))
    return(infer_all)
  })
  
  segments_df <- reactive({
    req(input$fileibdseg)
    fileibdseg <- input$fileibdseg
    ibdseg <- read.table(fileibdseg$datapath, header = TRUE, stringsAsFactors = FALSE)
    ibdseg <- ibdseg[, c("ID1", "ID2", "IBDType", "Chr", "StartMB", "StopMB")]
    colnames(ibdseg) <- c("ID1", "ID2", "IBDType", "chr", "start", "end")
    return(ibdseg)
  })
  
  all_seg_df <- reactive({
    req(input$fileallseg)
    fileallseg <- input$fileallseg
    allseg <- read.table(fileallseg$datapath, header = TRUE)
    allseg <- allseg[, c("Chr", "StartMB","StopMB")]
    colnames(allseg) <- c("chr", "start", "end")
    return(allseg)
  })
  
  filename <- reactive({
    req(input$fileinfer)
    fileinfer <- input$fileinfer
    file_prefix <- gsub(".seg", " ", fileinfer$name)
    return(file_prefix)
  })
 
  output$plot1 <- renderPlot({
    req(infer_df())
    individuals_all <- infer_df()
    individuals_all <- individuals_all[individuals_all$IBD1Seg >= input$IBD1Seg[1] & individuals_all$IBD1Seg <= input$IBD1Seg[2] & 
                                         individuals_all$IBD2Seg >= input$IBD2Seg[1] & individuals_all$IBD2Seg <= input$IBD2Seg[2],]
    d0 <- individuals_all$IBD2Seg>0.7
    d1.PO <- (!d0) & individuals_all$IBD1Seg+individuals_all$IBD2Seg>0.96 | (individuals_all$IBD1Seg+individuals_all$IBD2Seg>0.9 & individuals_all$IBD2Seg<0.08)
    d1.FS <- (!d0) & (!d1.PO) & individuals_all$PropIBD>0.35355 & individuals_all$IBD2Seg>=0.08
    d2 <- individuals_all$PropIBD>0.17678 & individuals_all$IBD1Seg+individuals_all$IBD2Seg<=0.9 & (!d1.FS)
    d3 <- individuals_all$PropIBD>0.08839 & individuals_all$PropIBD<=0.17678
    d4 <- individuals_all$PropIBD>0.04419 & individuals_all$PropIBD<=0.08839
    dU <- individuals_all$PropIBD>0 & individuals_all$PropIBD<=0.04419
    plot(individuals_all$IBD1Seg[dU], individuals_all$IBD2Seg[dU], type="p", col = "black", cex.lab=1.2,
         xlim=c(min(individuals_all$IBD1Seg), max(individuals_all$IBD1Seg)),
         ylim=c(min(individuals_all$IBD2Seg), max(individuals_all$IBD2Seg)),
         main = paste0("IBD Segments In Inferred ", prefix, " Relatives"),
         xlab=expression(paste("Length Proportion of IBD1 Segments (", pi[1], ")",sep="")),
         ylab=expression(paste("Length Proportion of IBD2 Segments (", pi[2], ")",sep="")))
    points(individuals_all$IBD1Seg[d0], individuals_all$IBD2Seg[d0], col="purple")
    points(individuals_all$IBD1Seg[d1.PO], individuals_all$IBD2Seg[d1.PO], col="red")
    points(individuals_all$IBD1Seg[d1.FS], individuals_all$IBD2Seg[d1.FS], col="green")
    points(individuals_all$IBD1Seg[d2], individuals_all$IBD2Seg[d2], col="blue")
    points(individuals_all$IBD1Seg[d3], individuals_all$IBD2Seg[d3], col="magenta")
    points(individuals_all$IBD1Seg[d4], individuals_all$IBD2Seg[d4], col="gold")
    abline(h = 0.08, col = "green", lty = 3, lwd = 2)
    abline(a = 0.96, b = -1, col = "red", lty = 3, lwd = 2)
    abline(a = 0.3535534, b = -0.5, col = "green", lty = 3, lwd = 2)
    abline(a = 0.1767767, b = -0.5, col = "blue", lty = 3, lwd = 2)
    abline(a = 0.08838835, b = -0.5, col = "magenta", lty = 3, lwd = 2)
    abline(a = 0.04419, b = -0.5, col = "gold", lty = 3, lwd = 2)
    allcolors <- c("purple", "red", "green", "blue", "magenta", "gold", "black")
    legend("topright", c("Inferred MZ", "Inferred PO", "Inferred FS", "Inferred 2nd", "Inferred 3rd", "Inferred 4th", "Inferred UN"),
           col=allcolors, text.col = allcolors, pch = 19, cex = 1.2)
  })
  output$click_info <- renderPrint({
    req(input$plot_click)
    req(infer_df())
    req(segments_df())
    req(all_seg_df())
    individuals_all <- infer_df()
    individuals_all <- individuals_all[individuals_all$IBD1Seg >= input$IBD1Seg[1] & individuals_all$IBD1Seg <= input$IBD1Seg[2] & 
                                         individuals_all$IBD2Seg >= input$IBD2Seg[1] & individuals_all$IBD2Seg <= input$IBD2Seg[2],]
    segments <- segments_df()
    all_seg <- all_seg_df()
    cat(paste0("IBD1Seg=",round(input$plot_click$x,4), "; IBD2Seg=", round(input$plot_click$y,4)))
    point.index <- which.min((individuals_all$IBD1Seg-input$plot_click$x)^2+(individuals_all$IBD2Seg-input$plot_click$y)^2) 
    if (!(abs(individuals_all[point.index, "IBD1Seg"]-input$plot_click$x) <= 0.01 & abs(individuals_all[point.index,"IBD2Seg"]-input$plot_click$y) <= 0.01)) {
      target.data <- NULL}
      else{
        segments$IBDType <- factor(segments$IBDType, levels = c("IBD0", "IBD1", "IBD2"))
        target.data <- segments[segments$ID1==individuals_all[point.index,"ID1"] & segments$ID2==individuals_all[point.index,"ID2"], ]
  }
      output$plot2 <- renderPlot({
        validate(
          need(nrow(target.data) > 0, "Please select a related pair")
        )
        theme_set(theme_bw(base_size = 18))
        g <- ggplot() +
          geom_rect(data = all_seg, aes(xmin = start, xmax = end, ymin = 0, max = 0.9), fill = 'white', color = "black", size = 0.85) + 
          geom_rect(data = target.data , aes(xmin = start, xmax = end, ymin = 0, ymax = 0.9, fill = IBDType)) + 
          geom_rect(data = all_seg, aes(xmin = start, xmax = end, ymin = 0, max = 0.9), color = "black", alpha = 0, size = 0.85) + 
          scale_fill_manual(values = c("IBD0" = "white", "IBD1" = "dodgerblue2", "IBD2" = "firebrick2"), drop = FALSE) + 
          facet_grid(chr ~ .) +
          labs(x = "Position (MB)", y = "", title=paste("IBD Segments between", target.data$ID1, "and", target.data$ID2, sep = " "))+
          theme(
            legend.position = "bottom", legend.key = element_rect(color = "black"),
            panel.background = element_rect(fill = 'grey80', color = 'grey80'), panel.border = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            axis.text.y = element_blank(), axis.ticks.y = element_blank()
          )
        print(g)
      })
    })
  }
