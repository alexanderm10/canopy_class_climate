# load in ring widths for BAI
library(dplR)

g1 <- read.csv("raw_input_files/Group1.csv")
g2 <- read.csv("raw_input_files/Group2.csv")
g3 <- read.csv("raw_input_files/Group3.csv")

g1.age <- g1$X
g2.age <- g2$X
g3.age <- g3$X

# divide by 10 to get cm
g1 <- g1[,2:ncol(g1)]/10
g2 <- g2[,2:ncol(g2)]/10
g3 <- g3[,2:ncol(g3)]/10

g1.bai <- bai.in(g1)
g2.bai <- bai.in(g2)
g3.bai <- bai.in(g3)

# Calculating the mean and CI
g1.bai.mean <- apply(g1.bai, 1, FUN=mean, na.rm=T)
g1.bai.lower <- apply(g1.bai, 1, FUN=quantile, 0.025, na.rm=T)
g1.bai.upper <- apply(g1.bai, 1, FUN=quantile, 0.975, na.rm=T)


g1.bai.graph <- data.frame(mean = g1.bai.mean,
                           LB = g1.bai.lower,
                           UB = g1.bai.upper,
                           Age = g1.age,
                           type="group1")


g2.bai.mean <- apply(g2.bai, 1, FUN=mean, na.rm=T)
g2.bai.lower <- apply(g2.bai, 1, FUN=quantile, 0.025, na.rm=T)
g2.bai.upper <- apply(g2.bai, 1, FUN=quantile, 0.975, na.rm=T)

g2.bai.graph <- data.frame(mean = g2.bai.mean,
                           LB = g2.bai.lower,
                           UB = g2.bai.upper,
                           Age = g1.age,
                           type="group2")


g3.bai.mean <- apply(g3.bai, 1, FUN=mean, na.rm=T)
g3.bai.lower <- apply(g3.bai, 1, FUN=quantile, 0.025, na.rm=T)
g3.bai.upper <- apply(g3.bai, 1, FUN=quantile, 0.975, na.rm=T)

g3.bai.graph <- data.frame(mean = g3.bai.mean,
                           LB = g3.bai.lower,
                           UB = g3.bai.upper,
                           Age = g3.age,
                           type="group3")

# Prelim plots
plot(g1.bai.graph$Age,g1.bai.graph$mean , type="l")
  lines(g1.bai.graph$LB, col="red")
  lines(g1.bai.graph$UB, col="red")

plot(g2.bai.graph$Age,g2.bai.graph$mean , type="l")
  lines(g2.bai.graph$LB, col="red")
  lines(g2.bai.graph$UB, col="red")
  
plot(g3.bai.graph$Age,g3.bai.graph$mean , type="l")
  lines(g3.bai.graph$LB, col="red")
  lines(g3.bai.graph$UB, col="red")

# combining groups together for ggplot graphing
all.groups.graph <- rbind(g1.bai.graph, g2.bai.graph, g3.bai.graph)
  
ggplot(data=all.groups.graph) + facet_wrap(~type) +
  geom_line(aes(x=Age,y=mean, color=type))+
  geom_ribbon(aes(x=Age, ymin=LB, ymax=UB, fill=type), alpha=0.4) +
  scale_x_continuous(expand=c(0,0), name="Age") +
  scale_y_continuous(expand=c(0,0), name=expression(bold(paste("BAI (mm"^"2", "Yr"^"-1",")")))) +
  
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
  guides(color=guide_legend(nrow=1)) +
  theme(axis.title.y= element_text(size=rel(2.1), face="bold"))+
  theme(axis.title.x= element_text(size=rel(2.1), face="bold"))+
  guides(color=guide_legend(title=""))

ggplot(data=all.groups.graph) + #facet_wrap(~type) +
  geom_line(aes(x=Age,y=mean, color=type))+
  #geom_ribbon(aes(x=Age, ymin=LB, ymax=UB, fill=type), alpha=0.4) +
  scale_x_continuous(expand=c(0,0), name="Age") +
  scale_y_continuous(expand=c(0,0), name=expression(bold(paste("BAI (mm"^"2", "Yr"^"-1",")")))) +
  
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
  guides(color=guide_legend(nrow=1)) +
  theme(axis.title.y= element_text(size=rel(2.1), face="bold"))+
  theme(axis.title.x= element_text(size=rel(2.1), face="bold"))+
  guides(color=guide_legend(title=""))