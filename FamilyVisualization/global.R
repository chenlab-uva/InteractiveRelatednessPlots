server <- function(input, output, session) {
  
  ped_df <- reactive({
    req(input$file1)
    fileinfer <- input$file1
    peddf <- read.table(fileinfer$datapath, stringsAsFactors = FALSE)[,c(1,2,5,6,7,8,9)]
    colnames(peddf) <- c("FID", "ID", "FA", "MO", "Sex", "Affected", "Status")
    peddf$Affected[peddf$Affected==-9 | peddf$Affected==0 | peddf$Affected==1] <- 0
    peddf$Affected[peddf$Affected==2] <- 1
    return(peddf)
  })


  kin_infer <- reactive({
    req(input$file2)
    kininfer <- input$file2
    kindf<- read.table(kininfer$datapath, header = TRUE, stringsAsFactors = FALSE)
    return(kindf)
  })
  
  one_fam <- eventReactive(input$EnterFID, {
    req(input$file1)
    req(input$file2)
    all_kin <- kin_infer()
    onedf <- all_kin[all_kin$FID==input$FamilyID,]
    pairs.list <- paste(onedf$ID1, onedf$ID2, sep = "_")
    updateSelectizeInput(session, "Pairs_ID1_ID2", label = "ID1_ID2", 
                         choices = c(Choose='', pairs.list), selected = NULL)
    return(onedf)
  })
  
  
  segments_df <- reactive({
    req(input$fileibdseg)
    fileibdseg <- input$fileibdseg
    ibdseg <- read.table(fileibdseg$datapath, header = TRUE, stringsAsFactors = FALSE)
    ibdseg <- ibdseg[, c("ID1", "ID2", "IBDType", "Chr", "StartMB", "StopMB")]
    return(ibdseg)
  })
  
  output$plot1 <- renderPlot({
    req(ped_df())
    req(one_fam)
    #req(input$FamilyID)
    one_fam_df <- one_fam( )
    ped <- ped_df()
    f <- unique(one_fam_df$FID)
    fam <- ped[ped$FID==f, ]
    shiny::validate(need(nrow(fam) > 0 , "Please select a valid Family ID"))
    if (all(fam[, c("FA", "MO")]==0)){
    g.empty <- make_empty_graph(n = nrow(fam), directed = FALSE)
    plot(g.empty, vertex.size=27, vertex.color=NA, vertex.label.cex=1, vertex.label.dist=1.6, vertex.label.degree= pi/2, 
         vertex.label.color="black", vertex.label= fam[,"ID"], edge.color=NA, layout=layout_with_fr(g.empty, grid="nogrid"), asp=0,
         vertex.shape=c("none", "square", "circle")[1+fam[,"Sex"]])
    } else {
      pedplot <- pedigree(id = fam$ID, dadid = fam$FA, momid = fam$MO, sex = as.numeric(fam$Sex),
                          affected = as.numeric(fam$Affected), status = as.numeric(fam$Status), famid = fam$FID, missid = 0)
      plot(pedplot[toString(f)], cex = 0.5, symbolsize = 2.8)
      }
  })
  
  output$plot2 <- renderPlot({
    req(ped_df())
    req(kin_infer())
    
    one_fam_df <- one_fam( )
    f <- unique(one_fam_df$FID)
    data <- kin_infer()

    ped <- ped_df()
 

    Inf.color <- c("purple", "red", "green", "blue", "yellow", NA)
    Inf.type <- c("Dup/MZ", "PO", "FS", "2nd", "3rd")
    data.fam <- merge(data, ped, by.x = c("FID", "ID1"), by.y = c("FID", "ID"))
    data.all <- merge(data.fam, ped, by.x = c("FID", "ID2"), by.y = c("FID", "ID"))[, c("FID", "ID1", "ID2", "Sex.x", "Sex.y", "InfType", "Error")]
    data.all[!data.all$InfType %in% Inf.type, "InfType"] <- 6
    for (i in 1:5) data.all[data.all$InfType == Inf.type[i], "InfType"] <- i
    data.all[data.all$Error==1,"Error"] <- 5
    data.all[data.all$Error==0.5|data.all$Error==0,"Error"] <- 1
    fam.sub <- data.all[data.all$FID==f,][, 2:7]
    
    shiny::validate(need(nrow(fam.sub) >0, "Please select a valid Family ID"))
    
    id <- unique(mapply(c, fam.sub[,c(1,3)], fam.sub[,c(2, 4)]))
    
    g <- graph_from_data_frame(d=fam.sub, vertices=id[, 1], directed=FALSE)
    plot(g, edge.width=fam.sub$Error, vertex.size=27, vertex.color=NA, vertex.label.cex=1,
         edge.color=Inf.color[as.numeric(fam.sub$InfType)], layout=layout_with_fr(g, grid="nogrid"), asp=0,
         vertex.shape=c("none", "square", "circle")[1+as.numeric(id[, 2])], margin=c(0.3,0,0,0))
    legend("bottomright", Inf.type, lty=1, col=Inf.color, text.col=Inf.color, cex=0.7, bty="n")

  })
  
  output$plot3 <- renderPlot({
    req(one_fam)
    req(input$fileinfer)
    req(input$fileibdseg)
    req(input$fileallseg)
    req(input$Pairs_ID1_ID2)
    
    one_fam_df <- one_fam( )
    f <- unique(one_fam_df$FID)
    
    ID1_ID2 <- input$Pairs_ID1_ID2
    ID1 <- unlist(strsplit(ID1_ID2,"_"))[1]
    ID2 <- unlist(strsplit(ID1_ID2,"_"))[2]
    
    
    fileallseg <- input$fileallseg
    allseg <- read.table(fileallseg$datapath, header = TRUE)
    allseg <- allseg[, c("Chr", "StartMB","StopMB")]
    all_seg <- allseg[allseg$Chr <= 22, ]
    
    fileinfer <- input$fileinfer # *.seg
    individuals_all <- read.table(fileinfer$datapath, header = TRUE, stringsAsFactors = FALSE)
    
    
    
    fileibdseg <- input$fileibdseg # *.gz
    ibdseg <- read.table(fileibdseg$datapath, header = TRUE, stringsAsFactors = FALSE)
    segments <- ibdseg[, c("ID1", "ID2", "IBDType", "Chr", "StartMB", "StopMB")]
    target.data <- segments[segments$ID1==ID1 & segments$ID2==ID2,   ]
    
    shiny::validate(need(nrow(target.data) >0, "No related inforamtion. Please select another pair"))
    
    Prop.IBD1 <- individuals_all[individuals_all$ID1==ID1 & individuals_all$ID2==ID2, "IBD1Seg"]
    Prop.IBD2 <- individuals_all[individuals_all$ID1==ID1 & individuals_all$ID2==ID2, "IBD2Seg"]
    
    theme_set(theme_bw(base_size = 16))
    g <- ggplot() +
      geom_rect(data = all_seg, aes(xmin = StartMB, xmax = StopMB, ymin = 0, max = 0.9), fill = 'white', color = "black", size = 0.85) + 
      geom_rect(data = target.data , aes(xmin = StartMB, xmax = StopMB, ymin = 0, ymax = 0.9, fill = IBDType)) + 
      geom_rect(data = all_seg, aes(xmin = StartMB, xmax = StopMB, ymin = 0, max = 0.9), color = "black", alpha = 0, size = 0.85) + 
      scale_fill_manual(values = c("IBD0" = "white", "IBD1" = "dodgerblue2", "IBD2" = "firebrick2"), drop = FALSE) + 
      facet_grid(Chr ~ .) + scale_x_continuous(expand  = c(0, 0), limits = c(0, NA)) + 
      labs(x = "Position (Mb)", y = "", title= paste0("IBD Segments between ", target.data$ID1, " and ", target.data$ID2, 
                                                      " (PropIBD1=",Prop.IBD1, ";PropIBD2=", Prop.IBD2,")")) + 
      theme(
        legend.position = "bottom", legend.key = element_rect(color = "black"),
        panel.background = element_rect(fill = 'grey80', color = 'grey80'), panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank()
      )
    print(g)
    
    
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
}
