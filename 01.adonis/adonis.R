library(reshape2)
library(vegan)

dt <- read.table("adonis.data", sep="\t", header=T, row.names = 1, check.names = F)

map <- as.matrix(read.table("adonis.group", sep="\t", header=T, check.names = F))
test <- merge(map, t(dt), by.x="samples", by.y="row.names")


all_group <- as.data.frame(unique(test$group))

out <- matrix(0,ncol=7,nrow=99)
colnames(out) = c("Df" ,"SumOfSqs", "R2", "F", "Pr(>F)", "group1", "group2")
j = 1

for (grp in 1:nrow(all_group)){
  for (grp2 in grp:nrow(all_group)){
    
    
    grp1 = as.character(all_group[grp,])
    grp2 = as.character(all_group[grp2,])
	
    if(grp1 != grp2){
	
    dt1 <- test[which(test$group == grp1 | test$group == grp2),]
    map1 <- dt1[,1:2]
    dt2 <- dt1[,3:length(dt1)]
    
    adonis2_result = adonis2(dt2 ~ group, map1, permutations=999, distance="canb")
    adonis2_result = adonis2_result[1,]
    
    out[j,1] = adonis2_result$Df
    out[j,2] = adonis2_result$SumOfSqs
    out[j,3] = adonis2_result$R2
    out[j,4] = adonis2_result$F
    out[j,5] = adonis2_result$`Pr(>F)`
    out[j,6] = grp1
    out[j,7] = grp2
    j = j + 1
    
    }
  }
}


write.csv(out,"adonis.csv")


