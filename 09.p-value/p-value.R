
dt <- read.table("abundance.f.182samples.and.182mags", header=T, row.names=1, sep="\t", check.names=F)
# 相对丰度
#--------------------------------
#dt =t(t(dt)/colSums(dt))*100
#--------------------------------
map <- read.table("group.txt", sep="\t", check.names = F, header=T)
test <- merge(map, t(dt), by.x="samples", by.y="row.names")



#out <- data.frame(row.names = c("mean1", "mean2", "p-value", "group"))
out <- data.frame(name=character(),m1=numeric(),m2=numeric(),p.value=numeric(), group=character())
dt_1 <- test[which(test$group == 'before'),]
dt_2 <- test[which(test$group == 'after'),]



calc <- function(dt1, dt2, paired){
  for(i in 3:ncol(dt1)){
    name <- colnames(dt1)[i]
    m1 <- mean(dt1[,i])
    m2 <- mean(dt2[,i])
    p <- wilcox.test(dt1[,i], dt2[,i],paired=paired)$p.value
    group <- paste(dt1[1,2], dt2[1,2], sep=".vs.")
    temp_row <- data.frame(name=name, m1=m1, m2=m2, p=p, group=group)
    #temp_row <- c(m1, m2, p, group)
    out <<- rbind(out,temp_row)
  }
  #print(out)
}

calc(dt_1, dt_2,  T)


write.table(out, file="p-value.relatived.csv", sep=",", row.names = FALSE)
