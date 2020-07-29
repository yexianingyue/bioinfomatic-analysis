#--------------------------------------------------------------------------------------
#                       << 每个物种的 p-value  >>
#--------------------------------------------------------------------------------------
# 先转为长列表，再看
library(ggpubr)
library(reshape2)


dt <- read.table("abundance.f.182samples.and.182mags", header=T, row.names=1, sep="\t", check.names=F)
map <- read.table("group.txt", sep="\t", check.names = F, header=T)
long <- melt(t(dt))
# 相对丰度
#--------------------------------
#dt =t(t(dt)/colSums(dt))*100
#--------------------------------

long <- merge(long, map, by.x="Var1", by.y="samples")
result = rbind()

for (i in row.names(dt)){
  temp <- subset(long, Var2==i);
  df_temp <- compare_means(value ~ group, data=temp, method="wilcox.test");
  df_temp$contig <- i;
  result <- rbind(result, df_temp);
  print(i)
}          

write.table(result, "p-value.csv",sep=",", row.names=F)
