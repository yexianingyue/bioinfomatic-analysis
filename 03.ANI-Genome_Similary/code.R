library(ggplot2)
library(reshape2)
library(doBy)
library(ggpubr)
library(patchwork)
library(ggplotify)
library(corrplot)
library(vegan)

setwd("D:/linksoftware/Rout")
mp<-read.table("richness_cures_group.txt", header = T, sep = "\t")
rare_tb=read.table("gene_presence_absence.Rtab", header = T, row.names = 1)
rare_tb=t(rare_tb)

rich_x4=rare_tb[rownames(rare_tb) %in% subset(mp, group1=="x4")[,1],]
rich_WQ8=rare_tb[rownames(rare_tb) %in% subset(mp, group1=="WQ8")[,1],]
rich_MLG15=rare_tb[rownames(rare_tb) %in% subset(mp, group1=="MLG15")[,1],]
rich_YC8=rare_tb[rownames(rare_tb) %in% subset(mp, group1=="YC8")[,1],]
rich_zh=rare_tb[rownames(rare_tb) %in% subset(mp, group1=="zh")[,1],]
get_plot_data=function(dd, nm=nm){
     dd.curve=specaccum(dd, method = "random")
     dd.curve.data=data.frame(Sites=dd.curve$sites, Richness=dd.curve$richness, SD=dd.curve$sd)
     dd.curve.data$label=rep(nm, nrow(dd.curve.data))
     dd.curve.data
 }
 
public.data=get_plot_data(rich_public, nm="public")
x4.data=get_plot_data(rich_x4, nm="x4")
WQ8.data=get_plot_data(rich_WQ8, nm="WQ8")
zh.data=get_plot_data(rich_zh, nm="zh")
YC8.data=get_plot_data(rich_YC8, nm="YC8")
MLG15.data=get_plot_data(rich_MLG15, nm="MLG15")

rich.all=rbind(public.data, x4.data, WQ8.data, zh.data,YC8.data,MLG15.data)

rich.plot=ggplot(rich.all, aes(x=Sites, y=Richness, color=label))+ geom_line() + geom_errorbar(aes(ymax = Richness + SD, ymin = Richness - SD), width = 0.25)+scale_color_brewer(palette = "Set3")+theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggsave(rich.plot, file="Figure_M1lh_richness_curve.pdf", width = 5, height = 3)