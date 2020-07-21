#---------------------------------------------------------------------------
#                              PCoA
#---------------------------------------------------------------------------
library(Aitchison)
library(vegan)
library(ggsci)
library(ggrepel)
library(ggplot2)

dt <- read.table("rpkm.sum.txt",header=T, sep="\t", check.names=F, row.names=1)
map <- read.table("group.txt", header=T, sep="\t", check.names=F)

otu.dist <- vegdist(t(dt),method="bray", binary=F)
#  计算组之间的差异
#  statistic R: 0.1496（R[-1,1]，>0表示组间有差异）
#  Significance: 0.001 P值
anosim(otu.dist, map$group, permutations = 999, distance="bray")


otu_pcoa<- cmdscale(otu.dist,eig=TRUE)
pc12 <- otu_pcoa$points[,1:2]
pc_importance<-round(otu_pcoa$eig/sum(otu_pcoa$eig)*100,digits = 2)
pc12 <- as.data.frame(pc12)
pc12$samples <- row.names(pc12)
data <- merge(pc12, map, by="samples")

ggplot(data,aes(x=V2,y=V1,colour=group)) + 
  geom_point(size=3) + labs(x=paste("PCoA 1 (", pc_importance[1],digits=4,"%)", sep=""), y=paste("PCoA 2 (", pc_importance[2],digits=4, "%)", sep=""),title="bray_curtis PCoA") #绘制点图并设定大小
theme_bw() +  #使用黑白主题# 
  geom_text(aes(label=samples,y=V2+0.01),size=4,vjust=0)#添加数据点的标签
guides(color=guide_legend(title = NULL)) + #去除图例标题
  theme(axis.title.x = element_text(size=15,family="sans"), # 修改x轴标题文本的属性
        axis.title.y = element_text(size=15,family="sans",angle=90), # 修改y轴标题文本的属性
        axis.text.y=element_text(size=12,family="sans"), # 修改x轴刻度标签文本的属性
        axis.text.x=element_text(size=12,family="sans"), # 修改y轴刻度标签文本的属性
        panel.grid = element_blank() #隐藏网格线
  )+
  xlab(paste("PCo1",pc_importance[1],"%"))+ylab(paste("PCo2",pc_importance[2],"%"))