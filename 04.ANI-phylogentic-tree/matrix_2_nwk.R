library(ggtree)
library(treeio) #就是解析各种进化树的包，nwk的话，可以加载
library(phytools)


dt <- read.table("ANI.matrix.txt", header=T, row.names=1, check.names=F, sep="\t")
hc <- hclust(as.dist(dt))

tree <- as.phylo(hc)
write.tree(tree, "tree.nwk")

