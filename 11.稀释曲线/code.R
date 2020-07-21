library(ggplot2)
library(reshape2)
library(doBy)
library(ggpubr)
#library(patchwork)
library(ggplotify)
library(corrplot)
#corrplot 0.84 loaded
library(vegan)
# 载入需要的程辑包：permute
# This is vegan 2.5-6


#读取数据
mp<-read.table("ll_M2_isolate_NCBI_MAG_group.txt", header = T, sep = "\t")
rare_tb=read.table("gene_presence_absence.Rtab", header = T, row.names = 1)

rare_tb=t(rare_tb)
View(mp)

# 选取每个样本
rich_public=rare_tb[rownames(rare_tb) %in% subset(mp, group=="public")[,1],]
rich_L29=rare_tb[rownames(rare_tb) %in% subset(mp, group=="L29")[,1],]
rich_MLG15=rare_tb[rownames(rare_tb) %in% subset(mp, group=="MLG15")[,1],]
rich_MLG6=rare_tb[rownames(rare_tb) %in% subset(mp, group=="MLG6")[,1],]

get_plot_data=function(dd, nm=nm){
    dd.curve=specaccum(dd, method = "random")
    dd.curve.data=data.frame(Sites=dd.curve$sites, Richness=dd.curve$richness, SD=dd.curve$sd)
    dd.curve.data$label=rep(nm, nrow(dd.curve.data))
    dd.curve.data
}

public.data=get_plot_data(rich_public, nm="public")
L29.data=get_plot_data(rich_L29, nm="L29")
MLG6.data=get_plot_data(rich_MLG6, nm="MLG6")
MLG15.data=get_plot_data(rich_MLG15, nm="MLG15")
rich.all=rbind(public.data,L29.data,MLG6.data,MLG15.data)
View(rich.all)

rich.plot=ggplot(rich.all, aes(x=Sites, y=Richness, color=label))+
  geom_line()+
  geom_errorbar(aes(ymax = Richness + SD, ymin = Richness - SD), width = 0.25)+
  scale_color_brewer(palette = "Set3")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

