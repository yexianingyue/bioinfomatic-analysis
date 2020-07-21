#---------------------------------------------------------------------------
#                              alpha-多样性
#---------------------------------------------------------------------------

library(vegan)
library(vegan)
library(ggplot2)
library(ggsignif)

#计算多样性指数
dt <- read.table("abundance.f.182samples.and.182mags", header=T, row.names=1, sep="\t", check.names=F)
map <- read.table("group.txt", sep="\t", check.names = F, header=T)
divdata <- t(dt)
Shannon.Wiener<-diversity(divdata,"shannon")
Simpson<-diversity(divdata,"simpson")
S <- specnumber(divdata) #物种累计曲线

#输出
write.table(Shannon.Wiener,"Shannon.Wiener.txt",sep="\t",col.names = F,quote=F)
write.table(Simpson, "Simpson.txt",sep="\t",col.names = F,quote=F)
write.table(S,"S.txt",sep = "\t",col.names = F,quote=F)

dt2 <- as.data.frame(read.table("Shannon.Wiener.txt", sep="\t", header=F))
dt2$dose <- as.factor(dt2$V1)

dt2 <- merge(dt2, map, by.x="V1", by.y="samples")

compaired <- list(c("before","after"))

ggplot(dt2, aes(y=V2, x=group, fill=group)) +
  geom_boxplot() +
  geom_signif(comparisons = compaired,step_increase = 0.1,map_signif_level = T,test=wilcox.test) + # 添加P值
  theme(axis.text.x = element_text(angle = 60, hjust = 1))# 旋转周标签角度

