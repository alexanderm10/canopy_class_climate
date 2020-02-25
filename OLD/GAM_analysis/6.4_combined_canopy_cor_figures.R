library(ggplot2)
source("poster_theme.R")
# Loading in all index-level correlations

load("processed_data/All_climate_corrs.Rdata")
load("processed_data/Dominant_climate_corrs.Rdata")
load("processed_data/Intermediate_climate_corrs.Rdata")
load("processed_data/Suppressed_climate_corrs.Rdata")

# combinings all data.frames into one object
index.cor <- rbind(all.cor.stack, dom.cor.stack, int.cor.stack, sup.cor.stack)
summary(index.cor)

load("processed_data/All_gs_chron_corrs.Rdata")
load("processed_data/Dominant_gs_chron_corrs.Rdata")
load("processed_data/Intermediate_gs_chron_corrs.Rdata")
load("processed_data/Suppressed_gs_chron_corrs.Rdata")

chron.cor <- rbind(all.sites.gs, dom.sites.gs, int.sites.gs, sup.sites.gs)
summary(chron.cor)

chron.cor$State <- factor(chron.cor$State, levels =c("MO", "IN", "OH", "MA", "ME"))

cbbPalette <- c("#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00")

pdf("figures/violin_climate_cor_megaplot.pdf", height=8, width=13)
ggplot(data=index.cor) + facet_grid(type~State)+

	geom_violin(aes(x=Canopy.Class, y=cor, fill=State), adjust = 2, trim=T) + ylim(-0.6,0.6)+
	
	geom_bar(data=chron.cor[chron.cor$type %in% c("precip", "tmean"),],aes(x=Canopy.Class, y=cor, fill=NA, color=State), stat="identity", position="dodge")+ ylim(-0.6,0.6)+

	geom_point(aes(x=Canopy.Class , y=biweight.mean), shape=95, size=15)+ ylim(-0.6,0.6)+
			
	geom_hline(yintercept=0.246, linetype="dashed") + 
  	geom_hline(yintercept=-0.246, linetype="dashed") + 	
  	geom_hline(yintercept=0, linetype="solid") +

	scale_color_manual(values=cbbPalette) +
  	scale_fill_manual(values=cbbPalette) +
  	poster.theme2 +
  	theme(axis.text.x = element_text(angle = 45, hjust = 1),panel.grid.major=element_blank(), panel.grid.minor= 	element_blank(), panel.border= element_blank(), panel.background= element_blank()) +
  	labs(title= "TR Index Climate Correlations", x="State", y=expression(bold(paste("Correlation Value (r)"))))
dev.off()
