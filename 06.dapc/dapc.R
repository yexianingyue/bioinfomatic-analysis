library("adegenet")

dt <- read.table("dapc.matrix", sep="\t", header=F, check.names = F, row.names = 1)
map <- read.table("dapc.map.txt", sep="\t", header=T, check.names = F)
dt_t <- t(dt)

data <- merge(map, dt_t, by.x="name", by.y="CAZY")
data2 <- subset(data, select=c(-CAZY, -Phylum, -Isloated))

test <- df2genind(data2, ploidy=50, ncode=200)
pop(test) <- data$Phylum
test@other <- subset(data, select=c(CAZY, Isloated))
#------------------------------
# dapc.test <- dapc(test, n.da = nPop(test) - 1, var.contrib = TRUE, scale = FALSE, n.pca = 300)
dapc.test <- dapc(test, n.da = 6, var.contrib = TRUE, scale = FALSE, n.pca = 300)
scatter(dapc.test, lwd=2, lty = 2, )
scatter(dapc.test, clabel=FALSE, legend=TRUE) # 加图例，关闭标签

 
