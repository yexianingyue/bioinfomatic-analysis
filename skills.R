dt =t(t(dt)/colSums(dt))*100 # 求数值占列的百分比
mean_sort = dt[(order(-rowSums(dt))), ] # 按照行的和值降序
aggregate(dm[,2:7],by=list(dm$project, dm$samples),sum)  # 分组求和
merge(dmm, map, by.x=c('Group.1', 'Group.2'), by.y=c('project', 'samples'))  # 多列合并
dmmm <- dmm[!duplicated(dmm[,c("Group.1", 'person')]),]  # 根据某几列去重
count(dmm, person, group1) # 分组计数，根据数据框dmm的 person， group1两列	library(dplyr)
dm[which(dm$person%in% c(person)),] # 根据某一列筛选	library(dplyr)
 sample(iris,3, replace=FALSE) # 随机抽样,抽3列，不放回。只对data.frame有作用
#---------去除含有0的行
x[x==0] <- NA
na.omit(x)
#---------------------------