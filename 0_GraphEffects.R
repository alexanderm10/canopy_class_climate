# Making functions so that all models can get graphed using the same parameters
cbbPalette <- c("#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00")

# plotting the size effect; standard dimensions = 8 tall, 5 wide
plot.size <- function(dat.plot){
  ggplot(data=dat.plot[dat.plot$Effect=="dbh.recon",]) +
    # ggtitle("Null Model") +
    facet_grid(Species~.) +
    geom_ribbon(aes(x=x, ymin=lwr.bai*100, ymax=upr.bai*100), alpha=0.5) +
    geom_line(aes(x=x, y=mean.bai*100)) +
    geom_hline(yintercept=100, linetype="dashed") +
    scale_x_continuous(expand=c(0,0)) +
    coord_cartesian(ylim=c(0, 1750)) +
    labs(x = expression(bold(paste("DBH (cm)"))), y = expression(bold(paste("Effect on BAI (%)"))))+
    theme(axis.line=element_line(color="black"), 
          panel.grid.major=element_blank(), 
          panel.grid.minor=element_blank(), 
          panel.border=element_blank(),  
          panel.background=element_rect(fill=NA, color="black"), 
          axis.text.x=element_text(angle=0, color="black", size=18), 
          axis.text.y=element_text(angle=0, color="black", size=18), 
          strip.text=element_text(face="bold", size=18),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5),
          legend.position="top",
          legend.key.size = unit(0.75, "cm"),
          legend.text = element_text(size=18),
          legend.key = element_rect(fill = "white")) + 
    #guides(color=guide_legend(nrow=1),)+
    theme(axis.title.x = element_text(size=22, face="bold"),
          axis.title.y= element_text(size=22, face="bold"))+
    theme(panel.spacing.x = unit(1.25,"lines"),
          panel.spacing.y = unit(1.25,"lines"))
}

plot.year <- function(dat.plot){
  ggplot(data=dat.plot[dat.plot$Effect=="Year",]) +
    # ggtitle("Null Model") +
    facet_wrap(~PlotID, scales="free_y") +
    geom_ribbon(aes(x=x, ymin=lwr.bai*100, ymax=upr.bai*100), alpha=0.5) +
    geom_line(aes(x=x, y=mean.bai*100)) +
    geom_hline(yintercept=100, linetype="dashed") +
    scale_x_continuous(expand=c(0,0)) +
    # coord_cartesian(ylim=c(0, 1750)) +
    labs(x = expression(bold(paste("Year"))), y = expression(bold(paste("Effect on BAI (%)")))) +
    theme(axis.line=element_line(color="black"), 
          panel.grid.major=element_blank(), 
          panel.grid.minor=element_blank(), 
          panel.border=element_blank(),  
          panel.background=element_rect(fill=NA, color="black"), 
          axis.text.x=element_text(angle=0, color="black", size=10), 
          axis.text.y=element_text(angle=0, color="black", size=10), 
          strip.text=element_text(face="bold", size=10),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5),
          legend.position="top",
          legend.key.size = unit(0.75, "cm"),
          legend.text = element_text(size=10),
          legend.key = element_rect(fill = "white")) + 
    #guides(color=guide_legend(nrow=1),)+
    theme(axis.title.x = element_text(size=12, face="bold"),
          axis.title.y= element_text(size=12, face="bold"))+
    theme(panel.spacing.x = unit(0.5,"lines"),
          panel.spacing.y = unit(0.5,"lines"))
}


# ---------------------------------------------
# The big one: 3-panel climate effects
# ---------------------------------------------

plot.climate <- function(dat.plot, canopy=F, species=F, ...){

  plot.base <- ggplot(data=dat.plot[dat.plot$Effect%in%c("tmean", "precip", "vpd.max"),]) +
    # facet_grid(.~Effect) +
    geom_hline(yintercept=100, linetype="dashed") +
    scale_y_continuous(limits=c(min(dat.plot$lwr.bai[dat.plot$Effect %in% c("tmean", "precip", "vpd.max")]*100, na.rm=T), max(dat.plot$upr.bai[dat.plot$Effect %in% c("tmean", "precip", "vpd.max")]*100, na.rm=T))) +
    theme(axis.line=element_line(color="black"), 
          panel.grid.major=element_blank(), 
          panel.grid.minor=element_blank(), 
          panel.border=element_blank(),  
          panel.background=element_rect(fill=NA, color="black"), 
          axis.ticks.length = unit(-0.5, "lines"),
          axis.text.x = element_text(margin=unit(c(1,1,1,1), "lines"), color="black", size=10),
          axis.text.y = element_text(margin=unit(c(1,1,1,1), "lines"), color="black", size=10),
          strip.text=element_text(face="bold", size=18),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5),
          legend.position="top",
          # legend.key.size = unit(0.75, "cm"),
          legend.text = element_text(size=12),
          legend.key = element_rect(fill = "white")) + 
    #guides(color=guide_legend(nrow=1),)+
    theme(axis.title.x = element_text(size=12, face="bold"),
          axis.title.y= element_text(size=12, face="bold")) +
    theme(panel.spacing.x = unit(1,"lines"),
          panel.spacing.y = unit(1,"lines"),
          strip.text.x = element_blank(),
          plot.background = element_rect(fill=NA, color=NA))
  
  if(species){ 
    plot.base <- plot.base + facet_grid(Species ~ Effect) 
  } else {
    plot.base <- plot.base + facet_grid(.~Effect)  
  }

  if(canopy){ 
    plot.base <- plot.base + 
      geom_ribbon(aes(x=x, ymin=lwr.bai*100, ymax=upr.bai*100, fill=Canopy.Class), alpha=0.5) +
      geom_line(aes(x=x, y=mean.bai*100, color=Canopy.Class)) +
      scale_fill_manual(values=c("#E69F00","#009E73", "#0072B2"))+
      scale_color_manual(values=c("#E69F00","#009E73", "#0072B2")) +
      theme(legend.title = element_blank())
  }  else {
    plot.base <- plot.base + 
      geom_ribbon(aes(x=x, ymin=lwr.bai*100, ymax=upr.bai*100), alpha=0.5) +
      geom_line(aes(x=x, y=mean.bai*100)) 
  }
  
  plot.tmean <- plot.base %+% subset(dat.plot, Effect=="tmean") + 
    labs(x = expression(bold(paste("Temperature ("^"o", "C)"))), y = expression(bold(paste("Effect on BAI (%)")))) +
    guides(fill=F, color=F) +
    theme(strip.text.y = element_blank(),
          axis.text.x = element_text(margin=unit(c(1,1,1,1), "lines"), color="black", size=10),
          axis.title.x = element_text(margin=unit(c(0,0,0,0), "lines"), color="black", size=12),
          plot.margin = unit(c(3.75,0.5, 0.5, 1), "lines"))
  
  
  if(canopy){
    plot.precip <- plot.base %+% subset(dat.plot, Effect=="precip") +     
      labs(x = expression(bold(paste("Precipitation (mm)"))), y = element_blank()) +
      theme(axis.text.y=element_blank(), 
            axis.ticks.y=element_line(unit(-0.5, units="lines")),
            strip.text.y = element_blank(),
            plot.margin = unit(c(1,0.5, 0.5, 0.5), "lines"))
  } else {
    plot.precip <- plot.base %+% subset(dat.plot, Effect=="precip") +     
      labs(x = expression(bold(paste("Precipitation (mm)"))), y = element_blank()) +
      theme(axis.text.y=element_blank(), 
            axis.ticks.y=element_line(unit(-0.5, units="lines")),
            strip.text.y = element_blank(),
            plot.margin = unit(c(3.75,0.5, 0.5, 0.5), "lines"))
    
  }

  plot.vpd <- plot.base %+% subset(dat.plot, Effect=="vpd.max") +     
    labs(x = expression(bold(paste("VPD (kPa)"))), y = element_blank()) +
    guides(fill=F, color=F) +
    theme(axis.text.y=element_blank(), 
          axis.ticks.y=element_line(unit(-0.5, units="lines")),
          plot.margin = unit(c(3.75,1, 0.5, 0.5), "lines"))
  
  cowplot::plot_grid(plot.tmean, plot.precip, plot.vpd, nrow=1, rel_widths = c(1.5, 1, 1.25))

}


plot.climate.site <- function(dat.plot, canopy=T, species=F, ...){
  if(species) stop("We're using the function that plots sites.  Can't do both species & site!")
  
  plot.base <- ggplot(data=dat.plot[dat.plot$Effect%in%c("tmean", "precip", "vpd.max"),]) +
    facet_grid(Site.Code~Effect) +
    geom_hline(yintercept=100, linetype="dashed") +
    scale_y_continuous(limits=c(min(dat.plot$lwr.bai[dat.plot$Effect %in% c("tmean", "precip", "vpd.max")]*100, na.rm=T), max(dat.plot$upr.bai[dat.plot$Effect %in% c("tmean", "precip", "vpd.max")]*100, na.rm=T))) +
    coord_cartesian(ylim=c(50, 200)) +
    theme(axis.line=element_line(color="black"), 
          panel.grid.major=element_blank(), 
          panel.grid.minor=element_blank(), 
          panel.border=element_blank(),  
          panel.background=element_rect(fill=NA, color="black"), 
          axis.ticks.length = unit(-0.5, "lines"),
          axis.text.x = element_text(margin=unit(c(1,1,1,1), "lines"), color="black", size=10),
          axis.text.y = element_text(margin=unit(c(1,1,1,1), "lines"), color="black", size=10),
          strip.text=element_text(face="bold", size=18),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5),
          legend.position="top",
          # legend.key.size = unit(0.75, "cm"),
          legend.text = element_text(size=12),
          legend.key = element_rect(fill = "white")) + 
    #guides(color=guide_legend(nrow=1),)+
    theme(axis.title.x = element_text(size=12, face="bold"),
          axis.title.y= element_text(size=12, face="bold")) +
    theme(panel.spacing.x = unit(0.5,"lines"),
          panel.spacing.y = unit(0.5,"lines"),
          strip.text.x = element_blank(),
          plot.background = element_rect(fill=NA, color=NA))
  
  if(canopy){ 
    plot.base <- plot.base + 
      geom_ribbon(aes(x=x, ymin=lwr.bai*100, ymax=upr.bai*100, fill=Canopy.Class), alpha=0.5) +
      geom_line(aes(x=x, y=mean.bai*100, color=Canopy.Class)) +
      scale_fill_manual(values=c("#E69F00","#009E73", "#0072B2"))+
      scale_color_manual(values=c("#E69F00","#009E73", "#0072B2")) +
      theme(legend.title = element_blank())
  }  else {
    plot.base <- plot.base + 
      geom_ribbon(aes(x=x, ymin=lwr.bai*100, ymax=upr.bai*100), alpha=0.5) +
      geom_line(aes(x=x, y=mean.bai*100)) 
  }
  
  plot.tmean <- plot.base %+% subset(dat.plot, Effect=="tmean") + 
    labs(x = expression(bold(paste("Temperature ("^"o", "C)"))), y = expression(bold(paste("Effect on BAI (%)")))) +
    guides(fill=F, color=F) +
    theme(strip.text.y = element_blank(),
          axis.text.x = element_text(margin=unit(c(1,1,1,1), "lines"), color="black", size=10),
          axis.title.x = element_text(margin=unit(c(0,0,0,0), "lines"), color="black", size=12),
          plot.margin = unit(c(3.75,0.5, 0.5, 1), "lines"))
  
  
  if(canopy){
    plot.precip <- plot.base %+% subset(dat.plot, Effect=="precip") +     
      labs(x = expression(bold(paste("Precipitation (mm)"))), y = element_blank()) +
      theme(axis.text.y=element_blank(), 
            axis.ticks.y=element_line(unit(-0.5, units="lines")),
            strip.text.y = element_blank(),
            plot.margin = unit(c(1,0.5, 0.5, 0.5), "lines"))
  } else {
    plot.precip <- plot.base %+% subset(dat.plot, Effect=="precip") +     
      labs(x = expression(bold(paste("Precipitation (mm)"))), y = element_blank()) +
      theme(axis.text.y=element_blank(), 
            axis.ticks.y=element_line(unit(-0.5, units="lines")),
            strip.text.y = element_blank(),
            plot.margin = unit(c(3.75,0.5, 0.5, 0.5), "lines"))
    
  }
  
  plot.vpd <- plot.base %+% subset(dat.plot, Effect=="vpd.max") +     
    labs(x = expression(bold(paste("VPD (kPa)"))), y = element_blank()) +
    guides(fill=F, color=F) +
    theme(axis.text.y=element_blank(), 
          axis.ticks.y=element_line(unit(-0.5, units="lines")),
          plot.margin = unit(c(3.75,1, 0.5, 0.5), "lines"))
  
  cowplot::plot_grid(plot.tmean, plot.precip, plot.vpd, nrow=1, rel_widths = c(1.5, 1, 1.25))
  
}

# ---------------------------------------------
