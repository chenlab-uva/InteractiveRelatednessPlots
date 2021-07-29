server <- function(input, output, session) {
  
  path <- reactiveValues(
    loc = NULL
  )
  
  prefix <- reactiveValues(
    name = NULL
  )
  
  click_val <- reactiveValues(clickx = NULL, clicky = NULL)
  
  observe({
    req(input$plot_click)
    click_val$clickx = input$plot_click$x
    click_val$clicky = input$plot_click$y
  })
  
  observeEvent(input$filechoose,{
    fullpath <- file.choose()
    file.base <- basename(fullpath)
    file.dir <- dirname(fullpath)
    output$text <- renderText({
      paste(file.base, "is loaded")
    })
    file.prefix <- gsub(".seg","", file.base)
    prefix$name <- file.prefix
    path$loc <- paste(file.dir, file.prefix, sep = "/")
    peddf <- allped_df()
    peddf <- peddf[peddf$Status==0,]
    family.size.df <- as.data.frame(table(peddf$FID))
    family.size.val <- unique(family.size.df$Freq)
    family.size.order <- family.size.val[order(family.size.val, decreasing = T)]
    updateTextInput(session, "FamilyIDtype", paste("Step 2: Please type an ID for the family to be visualized in", file.prefix, "data"), value = NULL)
    updateSelectizeInput(session, "FamilySize", paste("Or Step 2b: Please specify the family size in", file.prefix, "data"), choices = c(Choose='', family.size.order), selected = NULL)
  })
  
  observeEvent(input$FamilySize, {
    peddf <- allped_df()
    peddf <- peddf[peddf$Status==0,]
    family.size.df <- as.data.frame(table(peddf$FID))
    new.label <- paste("And then choose a family of size", input$FamilySize)
    updateSelectizeInput(session, "FamilyID", new.label, choices = c(Choose='', as.character(family.size.df[family.size.df$Freq==input$FamilySize, "Var1"])), selected = NULL)
  })
  
  observeEvent(input$FamilyIDtype, {
    updateTabsetPanel(session, "inTabset",selected = "panel1")
  })
  
  
  observeEvent(input$FamilyID, {
    updateTabsetPanel(session, "inTabset",selected = "panel2")
  })
  
  allped_df <- reactive({
    req(path$loc)
    splitpedfile <- paste(path$loc,"splitped.txt", sep = "")
    shiny::validate(
      need(file.exists(splitpedfile), paste0(prefix$name, "splitped.txt is missing"))
    )
    
    withProgress(message = 'Loading splitped.txt file',
                 detail = 'This may take a while...', value = 0, {
                   incProgress(1/1)
                   peddf <- read.table(splitpedfile, stringsAsFactors = FALSE)[,c(1,2,5,6,7,8,9)]
                 })
    colnames(peddf) <- c("FID", "ID", "FA", "MO", "Sex", "Affected", "Status")
    peddf$Affected[peddf$Affected==-9 | peddf$Affected==0 | peddf$Affected==1] <- 0
    peddf$Affected[peddf$Affected==2] <- 1
    return(peddf)
  })
  
  one_ped_df <- reactive({
    req(path$loc)
    req(input$FamilySize)
    req(input$FamilyID)
    req(allped_df())
    one_ped <- allped_df()
    one_ped <- one_ped[one_ped$FID == input$FamilyID, ]
    shiny::validate(
      need(nrow(one_ped) > 0, "Family ID doesn't exist. Please type a valid family ID")
    )
    return(one_ped)
  })
  
  
  one_ped_df_main <- reactive({
    req(path$loc)
    req(input$FamilyIDtype)
    req(allped_df())
    one_ped <- allped_df()
    one_ped <- one_ped[one_ped$FID == input$FamilyIDtype, ]
    shiny::validate(
      need(nrow(one_ped) > 0, "Family ID doesn't exist. Please type a valid family ID")
    )
    return(one_ped)
  })
  
  kin_infer <- reactive({
    req(path$loc)
    fileinfer <- paste(path$loc, ".seg", sep = "")
    shiny::validate(
      need(file.exists(fileinfer), paste0(prefix$name, ".seg", " is missing"))
    )
    
    withProgress(message = 'Loading .seg file',
                 detail = 'This may take a while...', value = 0, {
                   incProgress(1/1)
                   kindf <- read.table(fileinfer, header = TRUE, stringsAsFactors = FALSE)
                 })
    
    kindf <- kindf[, c("FID1","ID1", "FID2", "ID2","IBD1Seg", "IBD2Seg","InfType")]
    kindf <- kindf[kindf$InfType!="UN", ]
    shiny::validate(
      need(nrow(kindf) > 0, "No related samples")
    )
    return(kindf)
  })
  
  kin_infer_main <- reactive({
    req(path$loc)
    fileinfer <- paste(path$loc, ".seg", sep = "")
    shiny::validate(
      need(file.exists(fileinfer), paste0(prefix$name, ".seg", " is missing"))
    )
    withProgress(message = 'Loading .seg file',
                 detail = 'This may take a while...', value = 0, {
                   incProgress(1/1)
                   kindf <- read.table(fileinfer, header = TRUE, stringsAsFactors = FALSE)
                 })
    
    kindf <- kindf[, c("FID1","ID1", "FID2", "ID2","IBD1Seg", "IBD2Seg","InfType")]
    kindf <- kindf[kindf$InfType!="UN", ]
    shiny::validate(
      need(nrow(kindf) > 0, "No related samples")
    )
    return(kindf)
  })
  
  
  allseg_df <- reactive({
    req(path$loc)
    fileallseg <- paste0(path$loc,"allsegs.txt")
    shiny::validate(need(file.exists(fileallseg), paste0(prefix$name, "allsegs.txt is missing")))
    withProgress(message = 'Loading allsegs.txt file',
                 detail = 'This may take a while...', value = 0, {
                   incProgress(1/1)
                   allseg <- read.table(fileallseg, header = TRUE)
                 })
    allseg <- allseg[, c("Chr", "StartMB","StopMB")]
    all_seg <- allseg[allseg$Chr <= 22, ]
    return(all_seg)
  }) 
  
  segments_df <- reactive({
    req(path$loc)
    fileibdseg <- paste(path$loc, ".segments.gz", sep = "")
    shiny::validate(
      need(file.exists(fileibdseg), paste0(prefix$name, ".segments.gz is missing"))
    )
    withProgress(message = 'Loading .segments.gz file',
                 detail = 'This may take a while...', value = 0, {
                   incProgress(1/1)
                   ibdseg <- fread(fileibdseg, header=F, data.table=F)
                 })
    colnames(ibdseg) <- c("FID1","ID1","FID2","ID2","IBDType", "Chr", "StartMB","StopMB","StartSNP","StopSNP","N_SNP","Length") 
    ibdseg <- ibdseg[, c("FID1", "ID1", "FID2", "ID2", "IBDType", "Chr", "StartMB", "StopMB")]
    return(ibdseg)
  })
  
  coords_info  <- reactive({
    req(kin_infer())
    req(one_ped_df())
    req(input$FamilyID)
    f <- input$FamilyID
    data <- kin_infer()
    
    data.f <- data[data$FID1 ==f | data$FID2 == f, ]
    shiny::validate(need(nrow(data.f) > 0, "All samples are unrelated. Please choose another family ID."))
    
    data.f.sample <- unique(mapply(c, data.f[, c("FID1", "ID1")], data.f[, c("FID2", "ID2")]))
    
    if  (any(data.f.sample[, 1]!=f) & sum(data.f.sample[,1]!=f) >1 ) {
      data.other <- data.f.sample[data.f.sample[, 1]!=f, ]
      data.other.FID <- combn(data.other[,1],2)
      data.other.IID <- combn(data.other[,2],2)
      all.other.pairs <- NULL
      for (k in 1:dim(data.other.FID)[2]) {
        one.pair1 <- c(data.other.FID[1,k], data.other.IID[1,k], data.other.FID[2,k], data.other.IID[2,k])
        one.pair2 <- c(data.other.FID[2,k], data.other.IID[2,k], data.other.FID[1,k], data.other.IID[1,k])
        all.other.pairs <- rbind(all.other.pairs, one.pair1, one.pair2)
      }
      all.other.pairs <- as.data.frame(all.other.pairs)
      colnames(all.other.pairs) <- c("FID1", "ID1", "FID2", "ID2")
      data.f.other <- merge(data, all.other.pairs, by= c("FID1", "ID1", "FID2", "ID2"))
      data <- rbind(data.f, data.f.other)
    } else{
      data <- data.f
    }
    
    ped <- allped_df()
    Inf.color <- c("purple", "red", "green", "blue", "yellow", NA)
    Inf.type <- c("Dup/MZ", "PO", "FS", "2nd", "3rd")
    data.fam <- merge(data, ped, by.x = c("FID1", "ID1"), by.y = c("FID", "ID"))
    data.all <- merge(data.fam, ped, by.x = c("FID2", "ID2"), by.y = c("FID", "ID"))
    shiny::validate(need(nrow(data.all) >0, "Please select a valid Family ID with pedigree inforamtion"))
    data.all[!data.all$InfType %in% Inf.type, "InfType"] <- 6
    for (i in 1:5) data.all[data.all$InfType == Inf.type[i], "InfType"] <- i
    fam.sub <- data.all[, c("ID1", "ID2", "Sex.x", "Sex.y", "InfType")]
    id <- unique(mapply(c, fam.sub[,c(1,3)], fam.sub[,c(2, 4)]))
    g <- graph_from_data_frame(d=fam.sub, vertices=id[, 1], directed=FALSE)
    coords <- layout_with_fr(g, grid="nogrid")
    return(coords)
  })
  coords_info_main  <- reactive({
    req(kin_infer_main())
    req(input$FamilyIDtype)
    f <- input$FamilyIDtype
    data <- kin_infer_main()
    
    data.f <- data[data$FID1 ==f | data$FID2 == f, ]
    shiny::validate(need(nrow(data.f) > 0, "All samples are unrelated. Please choose another family ID."))
    data.f.sample <- unique(mapply(c, data.f[, c("FID1", "ID1")], data.f[, c("FID2", "ID2")]))
    
    if  (any(data.f.sample[, 1]!=f) & sum(data.f.sample[,1]!=f) >1 ) {
      data.other <- data.f.sample[data.f.sample[, 1]!=f, ]
      data.other.FID <- combn(data.other[,1],2)
      data.other.IID <- combn(data.other[,2],2)
      all.other.pairs <- NULL
      for (k in 1:dim(data.other.FID)[2]) {
        one.pair1 <- c(data.other.FID[1,k], data.other.IID[1,k], data.other.FID[2,k], data.other.IID[2,k])
        one.pair2 <- c(data.other.FID[2,k], data.other.IID[2,k], data.other.FID[1,k], data.other.IID[1,k])
        all.other.pairs <- rbind(all.other.pairs, one.pair1, one.pair2)
      }
      all.other.pairs <- as.data.frame(all.other.pairs)
      colnames(all.other.pairs) <- c("FID1", "ID1", "FID2", "ID2")
      data.f.other <- merge(data, all.other.pairs, by= c("FID1", "ID1", "FID2", "ID2"))
      data <- rbind(data.f, data.f.other)
    } else{
      data <- data.f
    }
    
    ped <- allped_df()
    Inf.color <- c("purple", "red", "green", "blue", "yellow", NA)
    Inf.type <- c("Dup/MZ", "PO", "FS", "2nd", "3rd")
    data.fam <- merge(data, ped, by.x = c("FID1", "ID1"), by.y = c("FID", "ID"))
    data.all <- merge(data.fam, ped, by.x = c("FID2", "ID2"), by.y = c("FID", "ID"))
    shiny::validate(need(nrow(data.all) >0, "Please select a valid Family ID with pedigree inforamtion"))
    data.all[!data.all$InfType %in% Inf.type, "InfType"] <- 6
    for (i in 1:5) data.all[data.all$InfType == Inf.type[i], "InfType"] <- i
    fam.sub <- data.all[, c("ID1", "ID2", "Sex.x", "Sex.y", "InfType")]
    id <- unique(mapply(c, fam.sub[,c(1,3)], fam.sub[,c(2, 4)]))
    g <- graph_from_data_frame(d=fam.sub, vertices=id[, 1], directed=FALSE)
    coords <- layout_with_fr(g, grid="nogrid")
    return(coords)
  })
  
  
  dist_func <- function(dot,dot1,dot2) {
    v1 <- dot1 - dot2
    v2 <- dot - dot1
    m <- cbind(v1,v2)
    d <- abs(det(m))/sqrt(sum(v1*v1))
    return(d)
  } 
  
  output$plot1 <- renderPlot({
    req(one_ped_df_main())
    req(allseg_df())
    req(segments_df())
    req(input$FamilyIDtype)
    f <- input$FamilyIDtype
    fam <- one_ped_df_main()
    if (all(fam[, c("FA", "MO")]==0)){
      g.empty <- make_empty_graph(n = nrow(fam), directed = FALSE)
      plot(g.empty, vertex.size=27, vertex.color=NA, vertex.label.cex=1, vertex.label.dist=1.6, vertex.label.degree= pi/2, 
           vertex.label.color="black", vertex.label= fam[,"ID"], edge.color=NA, layout=layout_with_fr(g.empty, grid="nogrid"), asp=0,
           vertex.shape=c("none", "square", "circle")[1+fam[,"Sex"]])
      mtext(paste("Reported Pedigree in Family", f, "(Non-Interactive)"), side = 3, line = -2, outer = TRUE, cex = 1.2)
    } else {
      pedplot <- pedigree(id = fam$ID, dadid = fam$FA, momid = fam$MO, sex = as.numeric(fam$Sex),
                          affected = as.numeric(fam$Affected), status = as.numeric(fam$Status), famid = fam$FID, missid = 0)
      #
      plot(pedplot[toString(f)], cex = 0.5, symbolsize = 2.8)
      mtext(paste("Reported Pedigree in Family", f, "(Non-Interactive)"), side = 3, line = -2, outer = TRUE, cex = 1.2)
    }
  })
  output$plot2 <- renderPlot({
    req(one_ped_df_main())
    req(coords_info_main())
    req(segments_df())
    coords_value <- coords_info_main()
    f <- input$FamilyIDtype
    data <- kin_infer_main()
    
    data.f <- data[data$FID1 ==f | data$FID2 == f, ]
    shiny::validate(need(nrow(data.f) > 0, "All samples are unrelated. Please choose another family ID."))
    data.f.sample <- unique(mapply(c, data.f[, c("FID1", "ID1")], data.f[, c("FID2", "ID2")]))
    
    if  (any(data.f.sample[, 1]!=f) & sum(data.f.sample[,1]!=f) >1 ) {
      data.other <- data.f.sample[data.f.sample[, 1]!=f, ]
      data.other.FID <- combn(data.other[,1],2)
      data.other.IID <- combn(data.other[,2],2)
      all.other.pairs <- NULL
      for (k in 1:dim(data.other.FID)[2]) {
        one.pair1 <- c(data.other.FID[1,k], data.other.IID[1,k], data.other.FID[2,k], data.other.IID[2,k])
        one.pair2 <- c(data.other.FID[2,k], data.other.IID[2,k], data.other.FID[1,k], data.other.IID[1,k])
        all.other.pairs <- rbind(all.other.pairs, one.pair1, one.pair2)
      }
      all.other.pairs <- as.data.frame(all.other.pairs)
      colnames(all.other.pairs) <- c("FID1", "ID1", "FID2", "ID2")
      data.f.other <- merge(data, all.other.pairs, by= c("FID1", "ID1", "FID2", "ID2"))
      data <- rbind(data.f, data.f.other)
    } else{
      data <- data.f
    }
    ped <- allped_df()
    Inf.color <- c("purple", "red", "green", "blue", "yellow", NA)
    Inf.type <- c("Dup/MZ", "PO", "FS", "2nd", "3rd")
    data.fam <- merge(data, ped, by.x = c("FID1", "ID1"), by.y = c("FID", "ID"))
    data.all <- merge(data.fam, ped, by.x = c("FID2", "ID2"), by.y = c("FID", "ID"))
    shiny::validate(need(nrow(data.all) >0, "Please select a valid Family ID with pedigree information"))
    uni.comb <- unique(paste(data.all$FID2,data.all$ID2, sep = "_"), paste(data.all$FID1,data.all$ID1))
    uni.comb.sec <- unlist(lapply(uni.comb,function(x) unlist(strsplit(x, "_"))[2]))
    shiny::validate(need(length(uni.comb)==length(unique(uni.comb.sec)), "Unique ID please."))
    data.all[!data.all$InfType %in% Inf.type, "InfType"] <- 6
    for (i in 1:5) data.all[data.all$InfType == Inf.type[i], "InfType"] <- i
    fam.sub <- data.all[, c("ID1", "ID2", "Sex.x", "Sex.y", "InfType")]
    id <- unique(mapply(c, fam.sub[,c(1,3)], fam.sub[,c(2, 4)]))
    g <- graph_from_data_frame(d=fam.sub, vertices=id[, 1], directed=FALSE)
    plot(g,vertex.size=20, vertex.color=NA, vertex.label.cex=1,
         edge.color=Inf.color[as.numeric(fam.sub$InfType)], layout=coords_value, asp=0,
         vertex.shape=c("none", "square", "circle")[1+as.numeric(id[, 2])], margin=c(0.3,0,0,0))
    legend("bottomright", Inf.type, lty=1, col=Inf.color, text.col=Inf.color, cex=0.7, bty="n")
    mtext(paste("Interactive Display of Relatedness with Clickable Lines"), side = 3, line = -2, outer = TRUE, cex = 1.2)
  })
  
  output$plot3 <- renderPlot({
    req(input$plot_click_main)
    req(coords_info_main())
    req(segments_df())
    f <- input$FamilyIDtype
    data <- kin_infer_main()
    
    data.f <- data[data$FID1 ==f | data$FID2 == f, ] 
    shiny::validate(need(nrow(data.f) > 0, "All samples are unrelated. Please choose another family ID."))
    data.f.sample <- unique(mapply(c, data.f[, c("FID1", "ID1")], data.f[, c("FID2", "ID2")]))
    
    if  (any(data.f.sample[, 1]!=f) & sum(data.f.sample[,1]!=f) >1 ) {
      data.other <- data.f.sample[data.f.sample[, 1]!=f, ]
      data.other.FID <- combn(data.other[,1],2)
      data.other.IID <- combn(data.other[,2],2)
      all.other.pairs <- NULL
      for (k in 1:dim(data.other.FID)[2]) {
        one.pair1 <- c(data.other.FID[1,k], data.other.IID[1,k], data.other.FID[2,k], data.other.IID[2,k])
        one.pair2 <- c(data.other.FID[2,k], data.other.IID[2,k], data.other.FID[1,k], data.other.IID[1,k])
        all.other.pairs <- rbind(all.other.pairs, one.pair1, one.pair2)
      }
      all.other.pairs <- as.data.frame(all.other.pairs)
      colnames(all.other.pairs) <- c("FID1", "ID1", "FID2", "ID2")
      #data.f.other <- merge(all.other.pairs, by.x = c("FID1", "ID1"), by.y = c("FID", "ID"))
      data.f.other <- merge(data, all.other.pairs, by= c("FID1", "ID1", "FID2", "ID2"))
      data <- rbind(data.f, data.f.other)
    } else{
      data <- data.f
    }
    ped <- allped_df()
    Inf.color <- c("purple", "red", "green", "blue", "yellow", NA)
    Inf.type <- c("Dup/MZ", "PO", "FS", "2nd", "3rd")
    data.fam <- merge(data, ped, by.x = c("FID1", "ID1"), by.y = c("FID", "ID"))
    data.all <- merge(data.fam, ped, by.x = c("FID2", "ID2"), by.y = c("FID", "ID"))
    data.all[!data.all$InfType %in% Inf.type, "InfType"] <- 6
    for (i in 1:5) data.all[data.all$InfType == Inf.type[i], "InfType"] <- i
    fam.sub <- data.all[, c("ID1", "ID2", "Sex.x", "Sex.y", "InfType")]
    shiny::validate(need(nrow(fam.sub) >0, "Please select a valid Family ID with pedigree information"))
    id <- unique(mapply(c, fam.sub[,c(1,3)], fam.sub[,c(2, 4)]))
    g <- graph_from_data_frame(d=fam.sub, vertices=id[, 1], directed=FALSE)
    coords_value <- coords_info_main()
    fam.sub.inf <- fam.sub[fam.sub$InfType <=5, ]
    fam.sub.inf.ID <- fam.sub.inf[,c(1,2)]
    dist.comb <- NULL
    loc.index <- NULL
    for (i in 1:nrow(fam.sub.inf.ID)){
      pair1 <- coords_value[which(V(g)$name==fam.sub.inf.ID[i,1]),]
      pair2 <- coords_value[which(V(g)$name==fam.sub.inf.ID[i,2]),]
      dot <- c(input$plot_click_main$x, input$plot_click_main$y)
      x.max <- max(coords_value[,1])
      x.min <- min(coords_value[,1])
      y.max <- max(coords_value[,2])
      y.min <- min(coords_value[,2])
      x.trans <- (x.max-x.min)*(input$plot_click_main$x + 1)/2 + x.min
      y.trans <- (y.max-y.min)*(input$plot_click_main$y + 1)/2 + y.min
      dot.trans <- c(x.trans, y.trans)
      dist.comb  <- c(dist.comb, dist_func(dot.trans,pair1,pair2))
      if ((dot.trans[1]-pair1[1])*(dot.trans[1]-pair2[1]) < 0 && (dot.trans[2]-pair1[2])*(dot.trans[2]-pair2[2]) < 0)
        loc.index <- c(loc.index,1) else {
          loc.index <- c(loc.index,0)
        }
    }
    
    
    ID.index.dist <- cbind(fam.sub.inf.ID, loc.index, dist.comb)
    ID.range <- ID.index.dist[loc.index==1,]
    selected.pair <- ID.range[which.min(ID.range[,4]),c(1,2)]
    shiny::validate(need(nrow(selected.pair) >0, "No close relatedness(up to 3rd degree) information. Please click a related pair"))
    ID1 <- selected.pair[1,1]
    ID2 <- selected.pair[1,2]
    
    all_seg <- allseg_df()
    individuals_all <- kin_infer_main()
    ibdseg <- segments_df()
    segments <- ibdseg[, c("FID1","ID1", "FID2", "ID2", "IBDType", "Chr", "StartMB", "StopMB")]
    target.data <- segments[(segments$ID1==ID1 & segments$ID2==ID2) | (segments$ID1==ID2 & segments$ID2==ID1) ,   ]
    
    shiny::validate(need(nrow(target.data) >0, "No related inforamtion. Please select another pair"))
    
    
    Prop.IBD1 <- formatC(individuals_all[(individuals_all$ID1==ID1 & individuals_all$ID2==ID2)| (individuals_all$ID1==ID2 & individuals_all$ID2==ID1), "IBD1Seg"], digits = 3, format = "f")
    Prop.IBD2 <- formatC(individuals_all[(individuals_all$ID1==ID1 & individuals_all$ID2==ID2) | (individuals_all$ID1==ID2 & individuals_all$ID2==ID1), "IBD2Seg"], digits = 3, format = "f")
    
    theme_set(theme_bw(base_size = 16))
    g <- ggplot() +
      geom_rect(data = all_seg, aes(xmin = StartMB, xmax = StopMB, ymin = 0, max = 0.9), fill = 'white', color = "black", size = 0.85) + 
      geom_rect(data = target.data , aes(xmin = StartMB, xmax = StopMB, ymin = 0, ymax = 0.9, fill = IBDType)) + 
      geom_rect(data = all_seg, aes(xmin = StartMB, xmax = StopMB, ymin = 0, max = 0.9), color = "black", alpha = 0, size = 0.85) + 
      scale_fill_manual(values = c("IBD0" = "white", "IBD1" = "dodgerblue2", "IBD2" = "firebrick2"), drop = FALSE) + 
      facet_grid(Chr ~ .) + scale_x_continuous(expand  = c(0, 0), limits = c(0, NA)) + 
      labs(x = "Position (Mb)", y = "", title=substitute(paste("IBD Segments for Relative Pair ", ID1," and ", ID2, " (", pi[1], "=", PropIBD1, ";", pi[2], "=", PropIBD2, ")"),
                                                         list(ID1 = target.data$ID1, ID2 = target.data$ID2, PropIBD1 = Prop.IBD1, PropIBD2 = Prop.IBD2))) + 
      theme(
        legend.position = "bottom", legend.key = element_rect(color = "black"),
        panel.background = element_rect(fill = 'grey80', color = 'grey80'), panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank()
      )
    print(g)
    output$dt1 <- renderDataTable({
      target.data
    })
    
  })
  
  
  
  output$plot4 <- renderPlot({
    req(one_ped_df())
    req(allseg_df())
    req(segments_df())
    req(input$FamilyID)
    f <- input$FamilyID
    fam <- one_ped_df()
    if (all(fam[, c("FA", "MO")]==0)){
      g.empty <- make_empty_graph(n = nrow(fam), directed = FALSE)
      plot(g.empty, vertex.size=27, vertex.color=NA, vertex.label.cex=1, vertex.label.dist=1.6, vertex.label.degree= pi/2, 
           vertex.label.color="black", vertex.label= fam[,"ID"], edge.color=NA, layout=layout_with_fr(g.empty, grid="nogrid"), asp=0,
           vertex.shape=c("none", "square", "circle")[1+fam[,"Sex"]])
      mtext(paste("Reported Pedigree in Family", f, "(Non-Interactive)"), side = 3, line = -2, outer = TRUE)
    } else {
      pedplot <- pedigree(id = fam$ID, dadid = fam$FA, momid = fam$MO, sex = as.numeric(fam$Sex),
                          affected = as.numeric(fam$Affected), status = as.numeric(fam$Status), famid = fam$FID, missid = 0)
      plot(pedplot[toString(f)], cex = 0.5, symbolsize = 2.8)
      mtext(paste("Reported Pedigree in Family", f, "(Non-Interactive)"), side = 3, line = -2, outer = TRUE, cex = 1.2)
    }
  })
  
  output$plot5 <- renderPlot({
    req(one_ped_df())
    req(coords_info())
    req(segments_df())
    coords_value <- coords_info()
    f <- input$FamilyID
    data <- kin_infer()
    
    data.f <- data[data$FID1 ==f | data$FID2 == f, ]
    shiny::validate(need(nrow(data.f) > 0, "All samples are unrelated. Please choose another family ID."))
    
    data.f.sample <- unique(mapply(c, data.f[, c("FID1", "ID1")], data.f[, c("FID2", "ID2")]))
    
    if  (any(data.f.sample[, 1]!=f) & sum(data.f.sample[,1]!=f) >1 ) {
      data.other <- data.f.sample[data.f.sample[, 1]!=f, ]
      data.other.FID <- combn(data.other[,1],2)
      data.other.IID <- combn(data.other[,2],2)
      all.other.pairs <- NULL
      for (k in 1:dim(data.other.FID)[2]) {
        one.pair1 <- c(data.other.FID[1,k], data.other.IID[1,k], data.other.FID[2,k], data.other.IID[2,k])
        one.pair2 <- c(data.other.FID[2,k], data.other.IID[2,k], data.other.FID[1,k], data.other.IID[1,k])
        all.other.pairs <- rbind(all.other.pairs, one.pair1, one.pair2)
      }
      all.other.pairs <- as.data.frame(all.other.pairs)
      colnames(all.other.pairs) <- c("FID1", "ID1", "FID2", "ID2")
      data.f.other <- merge(data, all.other.pairs, by= c("FID1", "ID1", "FID2", "ID2"))
      data <- rbind(data.f, data.f.other)
    } else{
      data <- data.f
    }

    
    
    ped <- allped_df()
    Inf.color <- c("purple", "red", "green", "blue", "yellow", NA)
    Inf.type <- c("Dup/MZ", "PO", "FS", "2nd", "3rd")
    data.fam <- merge(data, ped, by.x = c("FID1", "ID1"), by.y = c("FID", "ID"))
    data.all <- merge(data.fam, ped, by.x = c("FID2", "ID2"), by.y = c("FID", "ID"))
    shiny::validate(need(nrow(data.all) >0, "Please select a valid Family ID with pedigree information"))
    uni.comb <- unique(paste(data.all$FID2,data.all$ID2, sep = "_"), paste(data.all$FID1,data.all$ID1))
    uni.comb.sec <- unlist(lapply(uni.comb,function(x) unlist(strsplit(x, "_"))[2]))
    shiny::validate(need(length(uni.comb)==length(unique(uni.comb.sec)), "Unique ID please."))
    data.all[!data.all$InfType %in% Inf.type, "InfType"] <- 6
    for (i in 1:5) data.all[data.all$InfType == Inf.type[i], "InfType"] <- i
    fam.sub <- data.all[, c("ID1", "ID2", "Sex.x", "Sex.y", "InfType")]
    id <- unique(mapply(c, fam.sub[,c(1,3)], fam.sub[,c(2, 4)]))
    g <- graph_from_data_frame(d=fam.sub, vertices=id[, 1], directed=FALSE)
    plot(g,vertex.size=20, vertex.color=NA, vertex.label.cex=1,
         edge.color=Inf.color[as.numeric(fam.sub$InfType)], layout=coords_value, asp=0,
         vertex.shape=c("none", "square", "circle")[1+as.numeric(id[, 2])], margin=c(0.3,0,0,0))
    legend("bottomright", Inf.type, lty=1, col=Inf.color, text.col=Inf.color, cex=0.7, bty="n")
    mtext(paste("Interactive Display of Relatedness with Clickable Lines"), side = 3, line = -2, outer = TRUE, cex = 1.2)
    
  })
  
  output$plot6 <- renderPlot({
    req(input$plot_click)
    req(coords_info())
    req(segments_df())
    f <- input$FamilyID
    data <- kin_infer()
    
    data.f <- data[data$FID1 ==f | data$FID2 == f, ]
    shiny::validate(need(nrow(data.f) > 0, "All samples are unrelated. Please choose another family ID."))
    data.f.sample <- unique(mapply(c, data.f[, c("FID1", "ID1")], data.f[, c("FID2", "ID2")]))
    
    if  (any(data.f.sample[, 1]!=f) & sum(data.f.sample[,1]!=f) >1 ) {
      data.other <- data.f.sample[data.f.sample[, 1]!=f, ]
      data.other.FID <- combn(data.other[,1],2)
      data.other.IID <- combn(data.other[,2],2)
      all.other.pairs <- NULL
      for (k in 1:dim(data.other.FID)[2]) {
        one.pair1 <- c(data.other.FID[1,k], data.other.IID[1,k], data.other.FID[2,k], data.other.IID[2,k])
        one.pair2 <- c(data.other.FID[2,k], data.other.IID[2,k], data.other.FID[1,k], data.other.IID[1,k])
        all.other.pairs <- rbind(all.other.pairs, one.pair1, one.pair2)
      }
      all.other.pairs <- as.data.frame(all.other.pairs)
      colnames(all.other.pairs) <- c("FID1", "ID1", "FID2", "ID2")
      data.f.other <- merge(data, all.other.pairs, by= c("FID1", "ID1", "FID2", "ID2"))
      data <- rbind(data.f, data.f.other)
    } else{
      data <- data.f
    }
    ped <- allped_df()
    Inf.color <- c("purple", "red", "green", "blue", "yellow", NA)
    Inf.type <- c("Dup/MZ", "PO", "FS", "2nd", "3rd")
    data.fam <- merge(data, ped, by.x = c("FID1", "ID1"), by.y = c("FID", "ID"))
    data.all <- merge(data.fam, ped, by.x = c("FID2", "ID2"), by.y = c("FID", "ID"))
    data.all[!data.all$InfType %in% Inf.type, "InfType"] <- 6
    for (i in 1:5) data.all[data.all$InfType == Inf.type[i], "InfType"] <- i
    fam.sub <- data.all[, c("ID1", "ID2", "Sex.x", "Sex.y", "InfType")]
    shiny::validate(need(nrow(fam.sub) >0, "Please select a valid Family ID with pedigree information"))
    id <- unique(mapply(c, fam.sub[,c(1,3)], fam.sub[,c(2, 4)]))
    g <- graph_from_data_frame(d=fam.sub, vertices=id[, 1], directed=FALSE)
    coords_value <- coords_info()
    fam.sub.inf <- fam.sub[fam.sub$InfType <=5, ]
    fam.sub.inf.ID <- fam.sub.inf[,c(1,2)]
    dist.comb <- NULL
    loc.index <- NULL
    for (i in 1:nrow(fam.sub.inf.ID)){
      pair1 <- coords_value[which(V(g)$name==fam.sub.inf.ID[i,1]),]
      pair2 <- coords_value[which(V(g)$name==fam.sub.inf.ID[i,2]),]
      dot <- c(click_val$clickx, click_val$clicky)
      x.max <- max(coords_value[,1])
      x.min <- min(coords_value[,1])
      y.max <- max(coords_value[,2])
      y.min <- min(coords_value[,2])
      x.trans <- (x.max-x.min)*(click_val$clickx + 1)/2 + x.min
      y.trans <- (y.max-y.min)*(click_val$clicky + 1)/2 + y.min
      dot.trans <- c(x.trans, y.trans)
      dist.comb  <- c(dist.comb, dist_func(dot.trans,pair1,pair2))
      if ((dot.trans[1]-pair1[1])*(dot.trans[1]-pair2[1]) < 0 && (dot.trans[2]-pair1[2])*(dot.trans[2]-pair2[2]) < 0)
        loc.index <- c(loc.index,1) else {
          loc.index <- c(loc.index,0)
        }
    }
    
    ID.index.dist <- cbind(fam.sub.inf.ID, loc.index, dist.comb)
    ID.range <- ID.index.dist[loc.index==1,]
    selected.pair <- ID.range[which.min(ID.range[,4]),c(1,2)]
    
    shiny::validate(need(nrow(selected.pair) >0, "No close relatedness(up to 3rd degree) information. Please click a related pair"))
    
    ID1 <- selected.pair[1,1]
    ID2 <- selected.pair[1,2]
    
    all_seg <- allseg_df()
    individuals_all <- kin_infer()
    ibdseg <- segments_df()
    segments <- ibdseg[, c("FID1", "ID1", "FID2", "ID2", "IBDType", "Chr", "StartMB", "StopMB")]
    target.data <- segments[(segments$ID1==ID1 & segments$ID2==ID2) | (segments$ID1==ID2 & segments$ID2==ID1) ,   ]
    
    shiny::validate(need(nrow(target.data) >0, "No related inforamtion. Please select another pair"))
    
    
    Prop.IBD1 <- formatC(individuals_all[(individuals_all$ID1==ID1 & individuals_all$ID2==ID2)| (individuals_all$ID1==ID2 & individuals_all$ID2==ID1), "IBD1Seg"], digits = 3, format = "f")
    Prop.IBD2 <- formatC(individuals_all[(individuals_all$ID1==ID1 & individuals_all$ID2==ID2) | (individuals_all$ID1==ID2 & individuals_all$ID2==ID1), "IBD2Seg"], digits = 3, format = "f")
    
    theme_set(theme_bw(base_size = 16))
    g <- ggplot() +
      geom_rect(data = all_seg, aes(xmin = StartMB, xmax = StopMB, ymin = 0, max = 0.9), fill = 'white', color = "black", size = 0.85) + 
      geom_rect(data = target.data , aes(xmin = StartMB, xmax = StopMB, ymin = 0, ymax = 0.9, fill = IBDType)) + 
      geom_rect(data = all_seg, aes(xmin = StartMB, xmax = StopMB, ymin = 0, max = 0.9), color = "black", alpha = 0, size = 0.85) + 
      scale_fill_manual(values = c("IBD0" = "white", "IBD1" = "dodgerblue2", "IBD2" = "firebrick2"), drop = FALSE) + 
      facet_grid(Chr ~ .) + scale_x_continuous(expand  = c(0, 0), limits = c(0, NA)) + 
      labs(x = "Position (Mb)", y = "", title=substitute(paste("IBD Segments for Relative Pair ", ID1," and ", ID2, " (", pi[1], "=", PropIBD1, ";", pi[2], "=", PropIBD2, ")"),
                                                         list(ID1 = target.data$ID1, ID2 = target.data$ID2, PropIBD1 = Prop.IBD1, PropIBD2 = Prop.IBD2))) + 
      theme(
        legend.position = "bottom", legend.key = element_rect(color = "black"),
        panel.background = element_rect(fill = 'grey80', color = 'grey80'), panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank()
      )
    print(g)
    
    output$dt2 <- renderDataTable({
      target.data
    })
    
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
}
