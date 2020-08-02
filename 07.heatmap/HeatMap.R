########################ani heatmap

library(gplots)
require(lattice)
library(heatmap3)
library(heatmap.plus)
library(RColorBrewer)
rm(list=ls())
#下三角矩阵>补全矩阵（将ani.martirx矩阵将行名转置加列名，增加表头）
you <- read.table("inputdata.txt",sep = "\t",header = T,row.names = 1)
you<-as.matrix(you)
you[is.na(you)]=0
youzi= you + t(you)- diag(diag(you))
diag(youzi)<-rep(NA,length(diag(you)))
gg <- read.table("group.txt",sep = "\t",header = T,row.names = 1)
gg$gro = factor(gg$gro)
gg$gro1 = factor(gg$gro1)
color_matrix = cbind(brewer.pal(7,"Set1")[gg$gro],c('#f5af99','#eb6f5d')[gg$gro1])


color_matrix <- as.matrix(color_matrix) # 这边可以控制多少种分组,有多少列，就可以显示多少分组

colnames(color_matrix) = c('Type','sample') # 重命名列名


hm=heatmap3(youzi,
            col = colorRampPalette(c("#ed5119","#f47722","#fa9e28","#ffe6c1","#ffffff"))(100),
            key=T,densadj=0.1,denscol=T,trace="none",na.rm=T,na.color="white",
            cexCol=0.3,cexRow=0.3,margins=c(2,2),
            density.info=c("none"),
            ColSideColors = color_matrix, 
            RowSideColors = color_matrix,# 添加竖列的分组
            scale="none",keysize = 0.8,symkey = FALSE) #tow 


#(#00a087,#6ec8dc,#eb6f5d,#f5af99,#6376a0)
