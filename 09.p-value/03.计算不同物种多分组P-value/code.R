library(reshape2)
library(ggpubr)

# ----------------------------------------------------------------
#                   《函数定义区域》
# ----------------------------------------------------------------

# 一个分组对应比较其余分组【此处只支持三个分组，如果更多自行更改21-25行】
## 根据数据的不同，phylum需要自己改，改为分组的列名
zy = function(data){
  groups = unique(data$phylum)
  tt = rbind()
  # 循环所有分组【该物种的】
  for (i in groups[1:length(groups)]){
    temp_1 = data[which(data$phylum==i),]
    temp_1$temp_group = i
    
    temp_2 = data[which(data$phylum!=i),]
    
    # 取剩余两个分组的第一个字母作为temp_2$temp_group的value
    sgrps = unique(temp_2$phylum)
    a1 = substr(sgrps[1],1,1)
    a2 = substr(sgrps[2],1,1)
    a = paste(a1,a2,sep="")
    temp_2$temp_group = a
    
    #合并
    dt12 = rbind(temp_1, temp_2)
    c = compare_means(value ~ temp_group, dt12, method="wilcox.test")
    tt = rbind(c,tt)
  }
  tt
}


# ----------------------------------------------------------------
#                         《代码运行区域》
# ----------------------------------------------------------------

dt <- read.table("data.kegg.C.txt", sep="\t",header=T, row.names = 1,check.names = F)
map <- read.table("group.txt", sep="\t", check.names=F, header=T)

dm <- merge(map, t(dt), by.x='id', by.y='row.names')
long <- melt(dm)
result = rbind()

# 一个分组对应其余分组的比较
for (i in row.names(dt)){
  temp <- subset(long, variable==i);
  df_temp = zy(temp);
  df_temp$contig <- i;
  result <- rbind(result, df_temp);
  print(i)
}        
tt = result[,c(-1,-5,-6,-7,-8)] #删除没用的列，可有可无
write.table(tt, file='p-value.one2others.csv', row.names=F, sep=",")


result = rbind()
# 多分组两两比较
for (i in row.names(dt)){
  temp <- subset(long, variable==i);
  df_temp = compare_means(value ~ phylum, temp, method="wilcox.test");
  df_temp$contig <- i;
  result <- rbind(result, df_temp);
  print(i)
}
write.table(result, file='p-value.paire.csv', row.names=F, sep=",")



# 计算均值
k = aggregate(dm[,3:ncol(dm)],by=list(dm$phylum),mean)
k = t(k)
write.table(k, file='mean.csv', col.names=F, sep=",")
