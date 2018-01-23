# An example of stitching together separate ggplot graphs
{
  load(file.path("Data/analyses/analysis_TempExtent", "post-process_TempExtent.RData"))
  models.df <- data.frame(Model=unique(dat.ecosys[,"Model"]), Model.Order=unique(dat.ecosys[,"Model.Order"]))
  
  #   models.order <- models.df$Model.Order[order(models.df$Model.Order)]
  model.colors2 <- model.colors[model.colors$Model.Order %in% models.df$Model.Order,]
  model.colors2$Model.Order <- factor(model.colors2$Model.Order, levels=models.df$Model.Order[order(models.df$Model.Order)])  
  model.colors2 <- model.colors2[order(model.colors2$Model.Order),]
  
  
  colors.use <- as.vector(c(paste(model.colors2$color), "black", "gray30"))
  
  # -----------------
  # Creating a cheat data frame that lets values go off the graph
  # -----------------
  {
    ci.terms.graph <- ci.terms
    ci.terms.graph[!is.na(ci.terms.graph$mean.rel) & ci.terms.graph$mean.rel<(-0.55),"mean.rel"] <- NA 
    ci.terms.graph[!is.na(ci.terms.graph$lwr.rel) & ci.terms.graph$lwr.rel<(-0.55),"lwr.rel"] <- -0.55 
    ci.terms.graph[!is.na(ci.terms.graph$upr.rel) & ci.terms.graph$upr.rel<(-0.55),"upr.rel"] <- -0.55 
    ci.terms.graph[which(ci.terms.graph$mean.rel>0.75),"mean.rel"] <- NA 
    ci.terms.graph[!is.na(ci.terms.graph$upr.rel) & ci.terms.graph$lwr.rel>(0.75),"lwr.rel"] <- 0.75
    ci.terms.graph[!is.na(ci.terms.graph$upr.rel) & ci.terms.graph$upr.rel>(0.75),"upr.rel"] <- 0.75 
    ci.terms.graph[ci.terms.graph$Effect=="tair", "x"] <- ci.terms.graph[ci.terms.graph$Effect=="tair", "x"]-273.15
    
    ci.terms.graph <- merge(ci.terms.graph, models.df, all.x=T, all.y=F)
    summary(ci.terms.graph)
    
    # Grouping the kind and source of the data
    ci.terms.graph$Y.type <- as.factor(ifelse(ci.terms.graph$Model=="TreeRingRW", "RW", "NPP"))
    ci.terms.graph$data.type <- as.factor(ifelse(substr(ci.terms.graph$Model,1,8)=="TreeRing", "Tree Rings", "Model"))
    summary(ci.terms.graph)
    # summary(ci.terms.graph[ci.terms.graph$Model=="linkages",])
    
    # Creating a mask for values outside of the model drivers for that time period
    for(e in unique(ci.terms.graph$Extent)){
      yr <- as.numeric(strsplit(paste(e), "-")[[1]][1])
      
      tair    <- range(dat.ecosys2[dat.ecosys2$Model=="ed2" & dat.ecosys2$Year>=yr,"tair"   ], na.rm=T) - 273.15
      precipf <- range(dat.ecosys2[dat.ecosys2$Model=="ed2" & dat.ecosys2$Year>=yr,"precipf"], na.rm=T)
      co2     <- range(dat.ecosys2[dat.ecosys2$Model=="ed2" & dat.ecosys2$Year>=yr,"CO2"    ], na.rm=T)
      
      ci.terms.graph[ci.terms.graph$Extent==e & ci.terms.graph$Effect=="tair"   , "line.min"] <- tair   [1]
      ci.terms.graph[ci.terms.graph$Extent==e & ci.terms.graph$Effect=="precipf", "line.min"] <- precipf[1]
      ci.terms.graph[ci.terms.graph$Extent==e & ci.terms.graph$Effect=="CO2"    , "line.min"] <- co2    [1]
      
      ci.terms.graph[ci.terms.graph$Extent==e & ci.terms.graph$Effect=="tair"   , "line.max"] <- tair   [2]
      ci.terms.graph[ci.terms.graph$Extent==e & ci.terms.graph$Effect=="precipf", "line.max"] <- precipf[2]
      ci.terms.graph[ci.terms.graph$Extent==e & ci.terms.graph$Effect=="CO2"    , "line.max"] <- co2    [2]
    }
    ci.terms.graph$x.min <- ifelse(ci.terms.graph$x<ci.terms.graph$line.min, ci.terms.graph$x, ci.terms.graph$line.min)
    ci.terms.graph$x.max <- ifelse(ci.terms.graph$x>ci.terms.graph$line.max, ci.terms.graph$x, ci.terms.graph$line.max)
    ci.terms.graph$mask.min <- min(ci.terms.graph$lwr.rel, na.rm=T)
    ci.terms.graph$mask.max <- max(ci.terms.graph$upr.rel, na.rm=T)
    
    # Playing with the extent labels a bit so that "850-2010" is "all data" and "1901-2010" is left alone
    ci.terms.graph$Extent2 <- as.factor(ifelse(ci.terms.graph$Extent=="850-2010" | 
                                                 (ci.terms.graph$Extent=="1980-2010" & ci.terms.graph$Model=="TreeRingNPP") |
                                                 (ci.terms.graph$Extent=="1901-2010" & ci.terms.graph$Model=="TreeRingRW")
                                               , "All Data", paste(ci.terms.graph$Extent)))
    
    ci.terms.graph[ci.terms.graph$Extent2=="All Data", c("x.min", "x.max", "mask.min", "mask.max", "line.min", "line.max")] <- NA
    summary(ci.terms.graph)
    
    
    tree.rings.1901 <- ci.terms.graph[ci.terms.graph$Model=="TreeRingRW" & ci.terms.graph$Extent=="1901-2010",]
    tree.rings.1901$Extent2 <- as.factor("1901-2010")
    summary(tree.rings.1901)
    
    tree.rings.npp <- ci.terms.graph[ci.terms.graph$Model=="TreeRingNPP" & ci.terms.graph$Extent=="1980-2010",]
    tree.rings.npp$Extent2 <- as.factor("1980-2010")
    summary(tree.rings.npp)
    
    
    ci.terms.graph <- rbind(ci.terms.graph, tree.rings.1901, tree.rings.npp)
    ci.terms.graph$Extent3 <- recode(ci.terms.graph$Extent2, "'All Data'='0'; '1901-2010'='1'; '1980-2010'='2'")
    levels(ci.terms.graph$Extent3) <- c("0850-2010", "1901-2010", "1980-2010")
    summary(ci.terms.graph)
    
    ci.terms.graph <- ci.terms.graph[!(ci.terms.graph$Extent3=="0850-2010" & ci.terms.graph$data.type=="Tree Rings"),]
    levels(ci.terms.graph$Effect) <- c("Temperature", "Precipitation", "CO2", "Biomass")
    summary(ci.terms.graph)
  }
  # -----------------
  
  fig3.tair <- {
    ggplot(data=ci.terms.graph[ci.terms.graph$Effect == "Temperature",]) + 
      facet_grid(Extent3~Effect, scales="free_x") +
      geom_ribbon(aes(x=x, ymin=lwr.rel*100, ymax=upr.rel*100, fill=Model.Order), alpha=0.2) +
      geom_line(aes(x=x, y=mean.rel*100, color=Model.Order, linetype=Model.Order), size=0.75, alpha=0.8) +
      # Lower Shaded Region
      geom_ribbon(aes(x=x.min, ymin=mask.min*100, ymax=mask.max*100), alpha=0.3) +
      geom_vline(aes(xintercept=line.min), linetype="dashed") +
      # Upper Shaded Region
      geom_ribbon(aes(x=x.max, ymin=mask.min*100, ymax=mask.max*100), alpha=0.3) +
      geom_vline(aes(xintercept=line.max), linetype="dashed") +
      scale_x_continuous(expand=c(0,0), name=expression(bold(paste("Temperature ("^"o", "C)"))), breaks=c(10, 12.5, 15.0, 17.5)) +
      scale_y_continuous(name="NPP Contribution (% mean)", expand=c(0,0), breaks=c(-33, 0, 33, 66)) +
      guides(fill=F, color=F, linetype=F) +
      scale_fill_manual(values=colors.use) +
      scale_color_manual(values=colors.use) +
      scale_linetype_manual(values=c(rep("solid", length(colors.use)-1), "dashed")) +
      theme(strip.text.x=element_text(size=12, face="bold"),
            strip.text.y=element_blank()) + 
      theme(axis.line=element_line(color="black", size=0.5), 
            panel.grid.major=element_blank(), 
            panel.grid.minor=element_blank(), 
            panel.border=element_rect(fill=NA, color="black", size=0.5), 
            panel.background=element_blank(),
            panel.margin.x=unit(0, "lines"),
            panel.margin.y=unit(0, "lines"))  +
      theme(axis.text.y=element_text(color="black", size=10, margin=unit(c(0,1.5,0,0), "lines")),
            axis.text.x=element_text(color="black", size=10, margin=unit(c(1.5,0,0,0), "lines")), 
            axis.title.y=element_text(size=12, face="bold", margin=unit(c(0,0.5,0,0), "lines")),  
            axis.title.x=element_text(size=12, face="bold", margin=unit(c(0.65,0,0,0), "lines")),
            axis.ticks.length=unit(-0.5, "lines")) +
      theme(plot.margin=unit(c(0.5,0,0.5,0.5), "lines"))
    
  }
  fig3.precip <- {
    ggplot(data=ci.terms.graph[ci.terms.graph$Effect == "Precipitation",]) + 
      facet_grid(Extent3~Effect, scales="free_x") +
      geom_ribbon(aes(x=x, ymin=lwr.rel*100, ymax=upr.rel*100, fill=Model.Order), alpha=0.2) +
      geom_line(aes(x=x, y=mean.rel*100, color=Model.Order, linetype=Model.Order), size=0.75, alpha=0.8) +
      # Lower Shaded Region
      geom_ribbon(aes(x=x.min, ymin=mask.min*100, ymax=mask.max*100), alpha=0.3) +
      geom_vline(aes(xintercept=line.min), linetype="dashed") +
      # Upper Shaded Region
      geom_ribbon(aes(x=x.max, ymin=mask.min*100, ymax=mask.max*100), alpha=0.3) +
      geom_vline(aes(xintercept=line.max), linetype="dashed") +
      scale_x_continuous(expand=c(0,0), name=expression(bold(paste("Precipitation (mm yr"^"-1", ")")))) +
      scale_y_continuous(name="NPP Contribution (% mean)", expand=c(0,0)) +
      guides(fill=F, color=F, linetype=F) +
      scale_fill_manual(values=colors.use) +
      scale_color_manual(values=colors.use) +
      scale_linetype_manual(values=c(rep("solid", length(colors.use)-1), "dashed")) +
      theme(strip.text.x=element_text(size=12, face="bold"),
            strip.text.y=element_blank()) + 
      theme(axis.line=element_line(color="black", size=0.5), 
            panel.grid.major=element_blank(), 
            panel.grid.minor=element_blank(), 
            panel.border=element_rect(fill=NA, color="black", size=0.5), 
            panel.background=element_blank(),
            panel.margin.x=unit(0, "lines"),
            panel.margin.y=unit(0, "lines"))  +
      theme(axis.text.y=element_blank(),
            axis.text.x=element_text(color="black", size=10, margin=unit(c(1.5,0,0,0), "lines")), 
            axis.title.y=element_blank(),  
            axis.title.x=element_text(size=12, face="bold", margin=unit(c(0.5,0,0,0), "lines")),
            axis.ticks.length=unit(-0.5, "lines")) +
      theme(plot.margin=unit(c(0.5,0.0,0.5,0.5), "lines"))
    
  }
  fig3.co2 <- {
    ggplot(data=ci.terms.graph[ci.terms.graph$Effect == "CO2",]) + 
      facet_grid(Extent3~Effect, scales="free_x") +
      geom_ribbon(aes(x=x, ymin=lwr.rel*100, ymax=upr.rel*100, fill=Model.Order), alpha=0.2) +
      geom_line(aes(x=x, y=mean.rel*100, color=Model.Order, linetype=Model.Order), size=0.75, alpha=0.8) +
      # Lower Shaded Region
      geom_ribbon(aes(x=x.min, ymin=mask.min*100, ymax=mask.max*100), alpha=0.3) +
      geom_vline(aes(xintercept=line.min), linetype="dashed") +
      # Upper Shaded Region
      geom_ribbon(aes(x=x.max, ymin=mask.min*100, ymax=mask.max*100), alpha=0.3) +
      geom_vline(aes(xintercept=line.max), linetype="dashed") +
      scale_x_continuous(expand=c(0,0), name=expression(bold(paste("CO" ["2"], " (ppm)")))) +
      scale_y_continuous(name="NPP Contribution (% mean)", expand=c(0,0)) +
      guides(fill=guide_legend(title="Model"), 
             color=guide_legend(title="Model"), 
             linetype=guide_legend(title="Model")) +
      scale_fill_manual(values=colors.use) +
      scale_color_manual(values=colors.use) +
      scale_linetype_manual(values=c(rep("solid", length(colors.use)-1), "dashed")) +
      theme(legend.title=element_text(size=12, face="bold"),
            legend.text=element_text(size=10),
            legend.key=element_blank(),
            legend.key.size=unit(1.5, "lines"),
            legend.background=element_blank()) +
      theme(strip.text.x=element_text(size=12, face="bold"),
            strip.text.y=element_text(size=12, face="bold")) + 
      theme(axis.line=element_line(color="black", size=0.5), 
            panel.grid.major=element_blank(), 
            panel.grid.minor=element_blank(), 
            panel.border=element_rect(fill=NA, color="black", size=0.5), 
            panel.background=element_blank(),
            panel.margin.x=unit(0, "lines"),
            panel.margin.y=unit(0, "lines"))  +
      theme(axis.text.y=element_blank(),
            axis.text.x=element_text(color="black", size=10, margin=unit(c(1.5,0,0,0), "lines")), 
            axis.title.y=element_blank(),  
            axis.title.x=element_text(size=12, face="bold", margin=unit(c(0.79,0,0,0), "lines")),
            axis.ticks.length=unit(-0.5, "lines")) +
      theme(plot.margin=unit(c(0.5,0,0.5,0.5), "lines"))
    
  }
  
  png(file.path("Figures/analyses/analysis_TempExtent", "Fig2_Sensitivity_Rel_extent.png"), width=8, height=6, units="in", res=180)
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(nrow=1,ncol=3, widths=c(1.3,1,2))))
  print(fig3.tair  , vp = viewport(layout.pos.row = 1, layout.pos.col=1))
  print(fig3.precip, vp = viewport(layout.pos.row = 1, layout.pos.col=2))
  print(fig3.co2   , vp = viewport(layout.pos.row = 1, layout.pos.col=3))
  dev.off()
  
}