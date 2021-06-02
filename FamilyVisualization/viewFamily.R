library(kinship2)
library(igraph)
library(ggplot2)

options(scipen = 999)
options(shiny.maxRequestSize = 30*1024^2)
defaultpath <-  getwd()


ui <- fluidPage( 
  titlePanel(("Interface for Interactive Plot of Family Visualization")),
  sidebarLayout(position = "left",
                sidebarPanel(id = "sidebar",
                             textInput("path", "Path for KING --ibdseg output files", defaultpath),
                             textInput("prefix", "Prefix for KING --ibdseg inference", "t1dgc"),
                             textInput("FamilyID", "Family ID for the family to be visualized", "IMCHIP25"),
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


server <- function(input, output, session) {
  
  allped_df <- reactive({
    req(input$path)
    req(input$prefix)
    splitpedfile <- paste(input$path,"/",input$prefix, "splitped.txt", sep = "")
    validate(
      need(file.exists(splitpedfile), paste0(input$prefix, "splitped.txt is missing"))
    )
    peddf <- read.table(splitpedfile, stringsAsFactors = FALSE)[,c(1,2,5,6,7,8,9)]
    colnames(peddf) <- c("FID", "ID", "FA", "MO", "Sex", "Affected", "Status")
    peddf$Affected[peddf$Affected==-9 | peddf$Affected==0 | peddf$Affected==1] <- 0
    peddf$Affected[peddf$Affected==2] <- 1
    return(peddf)
  })
  
  
  
  one_ped_df <- eventReactive(input$EnterFID, {
    req(input$path)
    req(input$prefix)
    req(input$FamilyID)
    req(allped_df())
    one_ped <- allped_df()
    one_ped <- one_ped[one_ped$FID == input$FamilyID, ]
    validate(
      need(nrow(one_ped) > 0, "Please select a valid Family ID with pedigree information")
    )
    return(one_ped)
  })
  
  
  kin_infer <- eventReactive(input$EnterFID, {
    req(input$path)
    req(input$prefix)
    req(input$FamilyID)
    fileinfer <- paste(input$path,"/",input$prefix, ".seg", sep = "")
    
    validate(
      need(file.exists(fileinfer), paste0(input$prefix, ".seg", " is missing"))
    )
    
    kindf <- read.table(fileinfer, header = TRUE, stringsAsFactors = FALSE)
    kindf <- kindf[, c("FID1","ID1", "FID2", "ID2","InfType")]
    kindf <- kindf[kindf$FID1 == input$FamilyID | kindf$FID2 == input$FamilyID, ]
    return(kindf)
  })
  
  
  segments_df <- reactive({
    req(input$path)
    req(input$prefix)
    req(input$FamilyID)
    fileibdseg <- paste(input$path,"/",input$prefix, ".segments.gz", sep = "")
    validate(
      need(file.exists(fileibdseg), paste0(input$prefix, ".segments.gz is missing"))
    )
    ibdseg <- read.table(fileibdseg, header = TRUE, stringsAsFactors = FALSE)
    ibdseg <- ibdseg[, c("ID1", "ID2", "IBDType", "Chr", "StartMB", "StopMB")]
    return(ibdseg)
  })
  
  #family_ID <- eventReactive(input$EnterFID,{
    #fid <- input$FamilyID
    #return(fid)
  #})
  
  output$plot1 <- renderPlot({
    req(one_ped_df())
    req(input$FamilyID)
    f <- input$FamilyID
    fam <- one_ped_df()
    if (all(fam[, c("FA", "MO")]==0)){
      g.empty <- make_empty_graph(n = nrow(fam), directed = FALSE)
      plot(g.empty, vertex.size=27, vertex.color=NA, vertex.label.cex=1, vertex.label.dist=1.6, vertex.label.degree= pi/2, 
           vertex.label.color="black", vertex.label= fam[,"ID"], edge.color=NA, layout=layout_with_fr(g.empty, grid="nogrid"), asp=0,
           vertex.shape=c("none", "square", "circle")[1+fam[,"Sex"]])
      mtext(paste("Reported Pedigree in", f), side = 3, line = -2, outer = TRUE)
    } else {
      pedplot <- pedigree(id = fam$ID, dadid = fam$FA, momid = fam$MO, sex = as.numeric(fam$Sex),
                          affected = as.numeric(fam$Affected), status = as.numeric(fam$Status), famid = fam$FID, missid = 0)
      plot(pedplot[toString(f)], cex = 0.5, symbolsize = 2.8)
      mtext(paste("Reported Pedigree in", f), side = 3, line = -2, outer = TRUE)
    }
  })
  
  coords_info  <- eventReactive(input$EnterFID, {
    req(kin_infer())
    req(one_ped_df())
    f <- input$FamilyID
    #req(family_ID())
    #f <- family_ID()
    
    data <- kin_infer()
    ped <- allped_df()
    Inf.color <- c("purple", "red", "green", "blue", "yellow", NA)
    Inf.type <- c("Dup/MZ", "PO", "FS", "2nd", "3rd")
    data.fam <- merge(data, ped, by.x = c("FID1", "ID1"), by.y = c("FID", "ID"))
    data.all <- merge(data.fam, ped, by.x = c("FID2", "ID2"), by.y = c("FID", "ID"))
    shiny::validate(need(nrow(data.all) >0, "Please select a valid Family ID with pedigree inforamtion"))
    data.all[!data.all$InfType %in% Inf.type, "InfType"] <- 6
    for (i in 1:5) data.all[data.all$InfType == Inf.type[i], "InfType"] <- i
    fam.sub <- data.all[, c("ID1", "ID2", "Sex.x", "Sex.y", "InfType")]
    #shiny::validate(need(nrow(fam.sub) >0, "Please select a valid Family ID with pedigree inforamtion"))
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
  
  
  output$plot2 <- renderPlot({
    req(one_ped_df())
    req(coords_info())
    coords_value <- coords_info()
    f <- input$FamilyID
    #req(family_ID())
    #f <- family_ID()
    data <- kin_infer()
    ped <- allped_df()
    Inf.color <- c("purple", "red", "green", "blue", "yellow", NA)
    Inf.type <- c("Dup/MZ", "PO", "FS", "2nd", "3rd")
    data.fam <- merge(data, ped, by.x = c("FID1", "ID1"), by.y = c("FID", "ID"))
    data.all <- merge(data.fam, ped, by.x = c("FID2", "ID2"), by.y = c("FID", "ID"))
    #data.all <- data.all[, c("FID", "ID1", "FID2", "ID2", "Sex.x", "Sex.y", "InfType")]
    shiny::validate(need(nrow(data.all) >0, "Please select a valid Family ID with pedigree information"))
    data.all[!data.all$InfType %in% Inf.type, "InfType"] <- 6
    for (i in 1:5) data.all[data.all$InfType == Inf.type[i], "InfType"] <- i
    fam.sub <- data.all[, c("ID1", "ID2", "Sex.x", "Sex.y", "InfType")]
    id <- unique(mapply(c, fam.sub[,c(1,3)], fam.sub[,c(2, 4)]))
    g <- graph_from_data_frame(d=fam.sub, vertices=id[, 1], directed=FALSE)
    plot(g,vertex.size=20, vertex.color=NA, vertex.label.cex=1,
         edge.color=Inf.color[as.numeric(fam.sub$InfType)], layout=coords_value, asp=0,
         vertex.shape=c("none", "square", "circle")[1+as.numeric(id[, 2])], margin=c(0.3,0,0,0))
    legend("bottomright", Inf.type, lty=1, col=Inf.color, text.col=Inf.color, cex=0.7, bty="n")
    mtext(paste("Interactive Inferred Relatedness with Clickable Lines"), side = 3, line = -2, outer = TRUE)
    
  })
  
  output$click_info <- renderPrint({
    req(input$plot_click)
    req(one_ped_df())
    req(coords_info())
    f <- input$FamilyID
    data <- kin_infer()
    ped <- allped_df()
    Inf.color <- c("purple", "red", "green", "blue", "yellow", NA)
    Inf.type <- c("Dup/MZ", "PO", "FS", "2nd", "3rd")
    data.fam <- merge(data, ped, by.x = c("FID1", "ID1"), by.y = c("FID", "ID"))
    data.all <- merge(data.fam, ped, by.x = c("FID2", "ID2"), by.y = c("FID", "ID"))
    #data.all <- data.all[, c("FID", "ID1", "ID2", "Sex.x", "Sex.y", "InfType")]
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
      dot <- c(input$plot_click$x, input$plot_click$y)
      x.max <- max(coords_value[,1])
      x.min <- min(coords_value[,1])
      y.max <- max(coords_value[,2])
      y.min <- min(coords_value[,2])
      x.trans <- (x.max-x.min)*(input$plot_click$x + 1)/2 + x.min
      y.trans <- (y.max-y.min)*(input$plot_click$y + 1)/2 + y.min
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
    output$plot3 <- renderPlot({
      req(input$FamilyID)
      req(segments_df())
      f <- input$FamilyID
      #req(family_ID())
      #f <- family_ID()
      
      shiny::validate(need(nrow(selected.pair) >0, "No relatedness information. Please click a related pair"))
      
      ID1 <- selected.pair[1,1]
      ID2 <- selected.pair[1,2]
      
      fileallseg <- paste0(paste0(input$path,"/", input$prefix,"allsegs.txt"))
      validate(need(file.exists(fileallseg), paste0(input$prefix, "allseges.txt is missing")))
      
      allseg <- read.table(fileallseg, header = TRUE)
      allseg <- allseg[, c("Chr", "StartMB","StopMB")]
      all_seg <- allseg[allseg$Chr <= 22, ]
      
      fileinfer <- paste0(paste0(input$path,"/", input$prefix,".seg"))
      individuals_all <- read.table(fileinfer, header = TRUE, stringsAsFactors = FALSE)
      
      ibdseg <- segments_df()
      segments <- ibdseg[, c("ID1", "ID2", "IBDType", "Chr", "StartMB", "StopMB")]
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
        labs(x = "Position (Mb)", y = "", title=substitute(paste("IBD Segments between ", ID1," and ", ID2, " (", pi[1], "=", PropIBD1, ";", pi[2], "=", PropIBD2, ")"),
                                                           list(ID1 = target.data$ID1, ID2 = target.data$ID2, PropIBD1 = Prop.IBD1, PropIBD2 = Prop.IBD2))) + 
        theme(
          legend.position = "bottom", legend.key = element_rect(color = "black"),
          panel.background = element_rect(fill = 'grey80', color = 'grey80'), panel.border = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          axis.text.y = element_blank(), axis.ticks.y = element_blank()
        )
      print(g)
      
      
    })
    
    
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui, server)
