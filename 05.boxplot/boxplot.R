library(reshape2)
library(ggpubr)


paint <- function(dt, map,ylab=NULL,colors="#00afbb"){
  
  dt_t <- t(dt)
  dm <- merge(map, dt_t, by.x = 'sample',by.y='row.names')
  dt_long <- melt(dm)
  dt_long <- dt_long[which(dt_long$group_1 != "BV"),]
  ggboxplot(dt_long, x='variable',y='value',
           color=colors,
           x.text.angle=90,
           xlab = '',
           ylab = ylab
           )
  #ggplot(dt_long, aes(x='variable',y='value',file=)) +geom_boxplot()
}

dt <- read.table("./04.own.matrix.family.related",sep="\t", header=T,check.names = F,row.names = 1)
dt2 <- read.table("./05.own.matrix.family.absolute",sep="\t", header=T,check.names = F,row.names = 1)
map <- read.table("group2.txt", sep="\t",header=T,check.names = F)

paint(dt, map, "own.related")
paint(dt2, map, "own.absolute",'red')

