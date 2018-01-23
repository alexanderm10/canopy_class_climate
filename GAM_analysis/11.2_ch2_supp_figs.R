# Scripts for supplemental figure
# S1 <- Species Response curves
# S2 <- Species weights
# S3 <- Site Response curves
# S4 <- Site Weights
# S5 <- Canopy.Class * Site weights
library(ggplot2)
library(gridExtra)

###############################################################
# Supplemental Fig. 1 Species GAM response Curves
###############################################################
load("processed_data/gam1_response_graph.Rdata")
library(RColorBrewer)
summary(ci.terms.graph)
spp.use <- c("ACSA", "CARYA", "QUAL", "QURU", "TSCA", "PIST")

spp.colors <- read.csv("spp.Colors.csv", header=T)	
summary(spp.colors)	

group.fig <- spp.use
group.fig <- group.fig[order(group.fig)]
colors.use <- as.vector(c(paste(spp.colors[spp.colors$Species %in% group.fig, "color"])))
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Only plotting 2 major species at each site
S1.t <-		ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% c("tmean") & ci.terms.graph$group %in% spp.use,]) + 
			#facet_grid(~Effect, scales = "free") +
			geom_hline(yintercept=0, linetype="dashed")+
			geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai, fill=group), alpha=0.5) +
			geom_line(aes(x=x, y=mean.bai, color=group)) +
			scale_color_manual(values=brewer.pal(6, "Dark2")) +
			scale_fill_manual(values=brewer.pal(6, "Dark2"))+
			labs(x = expression(bold(paste("Temperature ("^"o", "C)"))), y = expression(bold(paste("Effect on BAI (mm"^"2","y"^"-1",")"))))+
			 theme(axis.line=element_line(color="black"), 
		panel.grid.major=element_blank(), 
		panel.grid.minor=element_blank(), 
		panel.border=element_blank(),  
		panel.background=element_blank(), 
		axis.text.x=element_text(angle=0, color="black", size=rel(1)), 
		#axis.text.y=element_text(angle=0, color="black", size=rel(1)), 
		strip.text=element_text(face="bold", size=rel(1.0)),
		axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="none",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=rel(1.1)),
        legend.key = element_rect(fill = "white")) + 
        guides(color=guide_legend(nrow=1, title=""), fill=guide_legend(nrow=1, title="")) +
        theme(axis.title.x = element_text(size=rel(1.1), face="bold"),
        axis.title.y= element_text(size=rel(1.1), face="bold"))+
        scale_x_continuous(expand=c(0,0)) +       #theme(axis.text.y=element_blank())	
		ylim(0,6) 

S1.p <- ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% c("precip") & ci.terms.graph$group %in% spp.use,]) + 
			#facet_grid(~Effect, scales = "free") +
			geom_hline(yintercept=0, linetype="dashed")+
			geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai, fill=group), alpha=0.5) +
			geom_line(aes(x=x, y=mean.bai, color=group)) +
			scale_color_manual(values=brewer.pal(6, "Dark2")) +
			scale_fill_manual(values=brewer.pal(6, "Dark2"))+
			labs(x = "Precipitation (mm)", y = element_blank())+
			 theme(axis.line=element_line(color="black"), 
		panel.grid.major=element_blank(), 
		panel.grid.minor=element_blank(), 
		panel.border=element_blank(),  
		panel.background=element_blank(), 
		axis.text.x=element_text(angle=0, color="black", size=rel(1)), 
		#axis.text.y=element_text(angle=0, color="black", size=rel(1)), 
		strip.text=element_text(face="bold", size=rel(1.0)),
		axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="none",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=rel(1.1)),
        legend.key = element_rect(fill = "white")) + 
        guides(color=guide_legend(ncol=1)) +
        theme(axis.title.x = element_text(size=rel(1.1), face="bold"),
        axis.title.y= element_text(size=rel(1.1), face="bold"))+
        scale_x_continuous(expand=c(0,0)) +       
        theme(axis.text.y=element_blank())	+
		ylim(0,6) 
		
S1.size <- ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% c("dbh.recon") & ci.terms.graph$group %in% spp.use,]) + 
			#facet_grid(~Effect, scales = "free") +
			geom_hline(yintercept=0, linetype="dashed")+
			geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai, fill=group), alpha=0.5) +
			geom_line(aes(x=x, y=mean.bai, color=group)) +
			scale_color_manual(values=brewer.pal(6, "Dark2")) +
			scale_fill_manual(values=brewer.pal(6, "Dark2"))+
			labs(x = "DBH (cm)", y = expression(bold(paste("Effect on BAI (mm"^"2","y"^"-1",")"))))+
			 theme(axis.line=element_line(color="black"), 
		panel.grid.major=element_blank(), 
		panel.grid.minor=element_blank(), 
		panel.border=element_blank(),  
		panel.background=element_blank(), 
		axis.text.x=element_text(angle=0, color="black", size=rel(1)), 
		#axis.text.y=element_text(angle=0, color="black", size=rel(1)), 
		strip.text=element_text(face="bold", size=rel(1.0)),
		axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="right",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=rel(1.1)),
        legend.key = element_rect(fill = "white")) + 
        guides(color=guide_legend(ncol=1, title=""), fill=guide_legend(title="")) +
        theme(axis.title.x = element_text(size=rel(1.1), face="bold"),
        axis.title.y= element_text(size=rel(1.1), face="bold"))+
        scale_x_continuous(expand=c(0,0)) +       #theme(axis.text.y=element_blank())	
		scale_y_continuous(expand=c(0,0)) 

# pdf("figures/prelim_figures/pub_figs/S1_spp_response.pdf", width= 13, height = 8.5)		
pdf("figures/submission1_figs/S1_spp_response.pdf", width= 13, height = 8.5)		
	grid.newpage()
	pushViewport(viewport(layout=grid.layout(nrow=1, ncol=3, widths=c(1.3,1,2))))
	print(S1.t, vp = viewport(layout.pos.row = 1, layout.pos.col=1))
  	print(S1.p + theme(plot.margin=unit(c(0.5,0,0.7,0),"lines")), vp = viewport(layout.pos.row = 1, layout.pos.col=2))
  	print(S1.size + theme(plot.margin=unit(c(0.5,0,0.7,0),"lines")), vp = viewport(layout.pos.row = 1, layout.pos.col=3))	
dev.off()

png(file.path("figures/Prelim_Figures/pub_figs/", "S1.png"), width=13, height=8.5, units="in", res=180)
	grid.newpage()
	pushViewport(viewport(layout=grid.layout(nrow=1, ncol=3, widths=c(1.3,1,2))))
	print(S1.t, vp = viewport(layout.pos.row = 1, layout.pos.col=1))
  	print(S1.p, vp = viewport(layout.pos.row = 1, layout.pos.col=2))
  	print(S1.size, vp = viewport(layout.pos.row = 1, layout.pos.col=3))	
dev.off()


###############################################################
# Supplemental Fig. 2 Site GAM response Curves
###############################################################
load("processed_data/gam4_response_graph.Rdata")
library(RColorBrewer)
library(car)
summary(ci.terms.graph)

ci.terms.graph$State <- recode(ci.terms.graph$Site,"'Howland'='ME';'Harvard'='MA';'Oak Openings Toledo'='OH';'Morgan Monroe State Park'='IN';'Missouri Ozark'='MO'")
ci.terms.graph$State <- factor(ci.terms.graph$State, levels=c("ME", "MA", "OH", "IN", "MO"))


cbbPalette <- c("#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00")
S2.t <- ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% c("tmean"), ]) + 
			#facet_wrap(~Effect, scales="free") +
			geom_ribbon(aes(x=x, ymin=exp(lwr), ymax=exp(upr), fill=State), alpha=0.5) +
			geom_line(aes(x=x, y=exp(mean), color=State))+
			geom_hline(yintercept=0, linetype="dashed") +
			scale_colour_manual("", values = cbbPalette) +
			scale_fill_manual("", values = cbbPalette) +
			theme_bw()+
			labs(x = expression(bold(paste("Temperature ("^"o", "C)"))), y = expression(bold(paste("Effect on BAI (mm"^"2","y"^"-1",")"))))+
			 theme(axis.line=element_line(color="black"), 
		panel.grid.major=element_blank(), 
		panel.grid.minor=element_blank(), 
		panel.border=element_blank(),  
		panel.background=element_blank(), 
		axis.text.x=element_text(angle=0, color="black", size=rel(1)), 
		#axis.text.y=element_text(angle=0, color="black", size=rel(1)), 
		strip.text=element_text(face="bold", size=rel(1.0)),
		axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="none",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=rel(1.1)),
        legend.key = element_rect(fill = "white")) + 
        guides(color=guide_legend(ncol=1)) +
        theme(axis.title.x = element_text(size=rel(1.1), face="bold"),
        axis.title.y= element_text(size=rel(1.1), face="bold"))+
        scale_x_continuous(expand=c(0,0)) +       #theme(axis.text.y=element_blank())	
		ylim(0,3) 

        


S2.p <- ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% c("precip"), ]) + 
			#facet_wrap(~Effect, scales="free") +
			geom_ribbon(aes(x=x, ymin=exp(lwr), ymax=exp(upr), fill=State), alpha=0.5) +
			geom_line(aes(x=x, y=exp(mean), color=State))+
			geom_hline(yintercept=0, linetype="dashed") +
			scale_colour_manual("", values = cbbPalette) +
			scale_fill_manual("", values = cbbPalette) +
			theme_bw()+
			labs(x = "Precip (mm)", y = element_blank())+
			 theme(axis.line=element_line(color="black"), 
		panel.grid.major=element_blank(), 
		panel.grid.minor=element_blank(), 
		panel.border=element_blank(),  
		panel.background=element_blank(), 
		axis.text.x=element_text(angle=0, color="black", size=rel(1)), 
		#axis.text.y=element_text(angle=0, color="black", size=rel(1)), 
		strip.text=element_text(face="bold", size=rel(1.0)),
		axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="none",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=rel(1.1)),
        legend.key = element_rect(fill = "white")) + 
        guides(color=guide_legend(ncol=1)) +
        theme(axis.title.x = element_text(size=rel(1.1), face="bold"),
        axis.title.y= element_text(size=rel(1.1), face="bold"))+
        scale_x_continuous(expand=c(0,0)) +       
		ylim(0,3)+
		theme(axis.text.y=element_blank())	 
		       
S2.size <- ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% c("dbh.recon"), ]) + 
			#facet_wrap(~Effect, scales="free") +
			geom_ribbon(aes(x=x, ymin=exp(lwr), ymax=exp(upr), fill=State), alpha=0.5) +
			geom_line(aes(x=x, y=exp(mean), color=State))+
			geom_hline(yintercept=0, linetype="dashed") +
			scale_colour_manual("", values = cbbPalette) +
			scale_fill_manual("", values = cbbPalette) +
			theme_bw()+
			labs(x = "DBH (cm)", y = element_blank())+
			 theme(axis.line=element_line(color="black"), 
		panel.grid.major=element_blank(), 
		panel.grid.minor=element_blank(), 
		panel.border=element_blank(),  
		panel.background=element_blank(), 
		axis.text.x=element_text(angle=0, color="black", size=rel(1)), 
		#axis.text.y=element_text(angle=0, color="black", size=rel(1)), 
		strip.text=element_text(face="bold", size=rel(1.0)),
		axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="right",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=rel(1.1)),
        legend.key = element_rect(fill = "white")) + 
        guides(color=guide_legend(ncol=1)) +
        theme(axis.title.x = element_text(size=rel(1.1), face="bold"),
        axis.title.y= element_text(size=rel(1.1), face="bold"))+
        scale_x_continuous(expand=c(0,0)) +       
		scale_y_continuous(expand=c(0,0))	 



# pdf("figures/Prelim_Figures/pub_figs/S2_combined.pdf", height = 8, width = 13)
pdf("figures/submission1_figs/S2_combined.pdf", height = 8, width = 13)
	grid.newpage()
	pushViewport(viewport(layout=grid.layout(nrow=1, ncol=3, widths=c(1.3,1,2))))
	print(S2.t , vp = viewport(layout.pos.row = 1, layout.pos.col=1))
  	print(S2.p + theme(plot.margin=unit(c(0.5,0,0.7,0),"lines")), vp = viewport(layout.pos.row = 1, layout.pos.col=2))
  	print(S2.size + theme(plot.margin=unit(c(0.5,0,0.7,0),"lines")), vp = viewport(layout.pos.row = 1, layout.pos.col=3))	
dev.off()

png(file.path("figures/Prelim_Figures/pub_figs/", "S2.png"), width=13, height=8.5, units="in", res=180)
	grid.newpage()
	pushViewport(viewport(layout=grid.layout(nrow=1,ncol=3, widths=c(1.3,1,2))))
	print(S2.t, vp = viewport(layout.pos.row = 1, layout.pos.col=1))
  	print(S2.p, vp = viewport(layout.pos.row = 1, layout.pos.col=2))
  	print(S2.size, vp = viewport(layout.pos.row = 1, layout.pos.col=3))
dev.off()


###############################################################
# Supplemental Fig. 3 Spp weights
###############################################################
load("processed_data/gam1_weights_graph.R")

S3 <- ggplot(data.graph[data.graph$group %in% spp.use,]) + facet_grid(group~State) +
		geom_ribbon(aes(x=Year, ymin=weight.tmean2 - weight.tmean2.SE, ymax=weight.tmean2 + weight.tmean2.SE), fill="red", alpha=0.25) +
		geom_ribbon(aes(x=Year, ymin=weight.precip2 - weight.precip2.SE, ymax=weight.precip2 + weight.precip2.SE), fill="blue", alpha=0.25) +
		geom_ribbon(aes(x=Year, ymin=weight.dbh.recon2- weight.dbh.recon2.SE, ymax=weight.dbh.recon2 + weight.dbh.recon2.SE), fill="darkgreen", alpha=0.25) +
	
		geom_line(aes(x=Year, y=weight.tmean2), size=1, color="red") +
		geom_line(aes(x=Year, y=weight.precip2), size=1, color="blue") +
		geom_line(aes(x=Year, y=weight.dbh.recon2), size=1, color="darkgreen")+
		labs(x=expression(bold(paste("Year"))), y = expression(bold(paste("Fraction Growth Limitation")))) +
	
	
	theme(axis.line=element_line(color="black"), 
		panel.grid.major=element_blank(), 
		panel.grid.minor=element_blank(), 
		panel.border=element_blank(),  
		panel.background=element_blank(), 
		axis.text.x=element_text(angle=0, color="black", size=rel(1)), 
		#axis.text.y=element_text(angle=0, color="black", size=rel(1)), 
		strip.text=element_text(face="bold", size=rel(1.0)),
		axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="right",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=rel(1.1)),
        legend.key = element_rect(fill = "white")) + 
        guides(color=guide_legend(ncol=1)) +
        theme(axis.title.x = element_text(size=rel(1.1), face="bold"),
        axis.title.y= element_text(size=rel(1.1), face="bold"))+
        scale_x_continuous(expand=c(0,0)) +       
		scale_y_continuous(expand=c(0,0))	 	

#pdf("figures/Prelim_Figures/pub_figs/S3.pdf", height = 8, width = 13)
pdf("figures/submission1_figs/S3.pdf", height = 8, width = 13)
	S3
dev.off()

png(file.path("figures/Prelim_Figures/pub_figs/", "S3.png"), width=13, height=8.5, units="in", res=180)
	S3

dev.off()


###############################################################
# Supplemental Fig. 4 Site Weights
###############################################################
load("processed_data/gam4_weights_graph.Rdata")

S4 <- ggplot(data.graph) + facet_grid(State~.) +
	
	geom_ribbon(aes(x=Year, ymin=weight.tmean2 - weight.tmean2.SE, ymax=weight.tmean2 + weight.tmean2.SE), alpha=0.25, fill="red") +
	geom_ribbon(aes(x=Year, ymin=weight.precip2 - weight.precip2.SE, ymax=weight.precip2 + weight.precip2.SE), alpha=0.25, fill="blue") +
	geom_ribbon(aes(x=Year, ymin=weight.dbh.recon2 - weight.dbh.recon2.SE, ymax=weight.dbh.recon2 + weight.dbh.recon2.SE), alpha=0.25, fill="darkgreen") +
	
	geom_line(aes(x=Year, y=weight.tmean2), size=1, color="red") +
	geom_line(aes(x=Year, y=weight.precip2), size=1, color="blue") +
	geom_line(aes(x=Year, y=weight.dbh.recon2), size=1, color="darkgreen")+
		
		labs(x=expression(bold(paste("Year"))), y = expression(bold(paste("Fraction Growth Limitation"))))+
	
	
	theme(axis.line=element_line(color="black"), 
		panel.grid.major=element_blank(), 
		panel.grid.minor=element_blank(), 
		panel.border=element_blank(),  
		panel.background=element_blank(), 
		axis.text.x=element_text(angle=0, color="black", size=rel(1)), 
		#axis.text.y=element_text(angle=0, color="black", size=rel(1)), 
		strip.text=element_text(face="bold", size=rel(1.0)),
		axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="right",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=rel(1.1)),
        legend.key = element_rect(fill = "white")) + 
        guides(color=guide_legend(ncol=1)) +
        theme(axis.title.x = element_text(size=rel(1.1), face="bold"),
        axis.title.y= element_text(size=rel(1.1), face="bold"))+
        scale_x_continuous(expand=c(0,0)) +       
		scale_y_continuous(expand=c(0,0))	 	
	
#pdf("figures/Prelim_Figures/pub_figs/S4.pdf", height = 8, width = 13)
pdf("figures/submission1_figs/S4.pdf", height = 8, width = 13)
	S4
dev.off()

png(file.path("figures/Prelim_Figures/pub_figs/", "S4.png"), width=13, height=8.5, units="in", res=180)
	S4

dev.off()

###############################################################
# Supplemental Fig. 5 Site * Canopy.Class Weights
###############################################################
load("processed_data/gam2_weights_graph_withSites.Rdata")
data.graph$Canopy.Class <- recode(data.graph$Canopy.Class, "'D'='Dominant';'I'='Intermediate';'S'='Suppressed'")

S5 <- ggplot(data.graph) + facet_grid(Canopy.Class~State) +
	
	geom_ribbon(aes(x=Year, ymin=weight.tmean2 - weight.tmean2.SE, ymax=weight.tmean2 + weight.tmean2.SE), alpha=0.25, fill="red") +
	geom_ribbon(aes(x=Year, ymin=weight.precip2 - weight.precip2.SE, ymax=weight.precip2 + weight.precip2.SE), alpha=0.25, fill="blue") +
	geom_ribbon(aes(x=Year, ymin=weight.dbh.recon2 - weight.dbh.recon2.SE, ymax=weight.dbh.recon2 + weight.dbh.recon2.SE), alpha=0.25, fill="darkgreen") +
	
	geom_line(aes(x=Year, y=weight.tmean2), size=1, color="red") +
	geom_line(aes(x=Year, y=weight.precip2), size=1, color="blue") +
	geom_line(aes(x=Year, y=weight.dbh.recon2), size=1, color="darkgreen")+
		
		labs(x=expression(bold(paste("Year"))), y = expression(bold(paste("Fraction Growth Limitation"))))+
	
	
	theme(axis.line=element_line(color="black"), 
		panel.grid.major=element_blank(), 
		panel.grid.minor=element_blank(), 
		panel.border=element_blank(),  
		panel.background=element_blank(), 
		axis.text.x=element_text(angle=0, color="black", size=rel(1)), 
		#axis.text.y=element_text(angle=0, color="black", size=rel(1)), 
		strip.text=element_text(face="bold", size=rel(1.0)),
		axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="right",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=rel(1.1)),
        legend.key = element_rect(fill = "white")) + 
        guides(color=guide_legend(ncol=1)) +
        theme(axis.title.x = element_text(size=rel(1.1), face="bold"),
        axis.title.y= element_text(size=rel(1.1), face="bold"))+
        scale_x_continuous(expand=c(0,0)) +       
		scale_y_continuous(expand=c(0,0))	 	

#pdf("figures/Prelim_Figures/pub_figs/S5.pdf", height = 8, width = 13)
pdf("figures/submission1_figs/S5.pdf", height = 8, width = 13)
	S5
dev.off()

png(file.path("figures/Prelim_Figures/pub_figs/", "S5.png"), width=13, height=8.5, units="in", res=180)
	S5

dev.off()

###############################################################
# Supplemental Fig. 6. SEA figure
###############################################################

load("processed_data/sea_output_combo.Rdata")
summary(sea.output.combo)


# pdf("figures/Prelim_Figures/pub_figs/S6.pdf", width=13, height=8.5)
pdf("figures/submission1_figs/S6.pdf", width=13, height=8.5)
ggplot(data=sea.output.combo[sea.output.combo$type=="chron" & sea.output.combo$lag==0,]) + facet_grid(Mark~State) +
  #geom_line(aes(x=lag, y=se, color=Canopy.Class, group=Canopy.Class),size=0.5)+
  geom_point(aes(x=lag, y=se, color=Canopy.Class,position="identity"), size=2)+
  
  scale_color_manual(values=c("#0072B2", "#009E73", "#E69F00"))+
  geom_hline(yintercept=0, linetype="dashed")+
  #scale_color_manual(values=c("red", "burlywood3", "darkgreen", "blue")) +
  labs(x="Lag", y="Scaled BA-Inex")+
  theme(axis.line=element_line(color="black"), 
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(), 
        panel.border=element_blank(),  
        panel.background=element_blank(), 
        axis.text.x=element_text(angle=0, color="black", size=rel(1)), 
        axis.text.y=element_text(angle=0, color="black", size=rel(1)), 
        strip.text=element_text(face="bold", size=rel(1.0)),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="top",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=rel(1.1)),
        legend.key = element_rect(fill = "white")) + 
  guides(color=guide_legend(nrow=1, title="")) +
  theme(axis.title.y= element_text(size=rel(1.1), face="bold")) +
  theme(axis.title.x= element_text(size=rel(1.1), face="bold")) +
  scale_y_continuous(expand=c(0,0), limits=c(-2,2))

dev.off()
