library(ggplot2)
library(reshape2)
library(doBy)
library(ggpubr)
#library(patchwork)
library(ggplotify)
library(corrplot)
#corrplot 0.84 loaded
library(vegan)
# 载入需要的程辑包：permute
# This is vegan 2.5-6

dt <- read.table("clu_3.matrix", sep="\t", check.names=F, header=T, row.names=1)

dtm = as.matrix(t(dt)) # 用作pan gene

get_core_data=function(core_dt, label='core'){

  core_dt[core_dt==0] <- NA
  COUNT = 100 # 抽样次数
  core_result = rbind()
  
  for (i in 1:length(core_dt)){
    per_sampling = c()
    for (j in 1:COUNT){ #抽样多少次
      temp_dt = sample(core_dt, i, replace = FALSE)
      core_num = nrow(na.omit(temp_dt))
      per_sampling = c( per_sampling, core_num)
    }
    std = sd(per_sampling) # 计算标准差
    richness = mean(per_sampling)
    sites = i
    label = "core"
    temp_result = data.frame(Sites=sites, Richness=richness, SD=std, label=label)
    core_result = rbind(core_result, temp_result)
  }
  core_result
}

# pan gene
get_pan_data=function(dd, nm=nm){
  dd.curve=specaccum(dd, method = "random")
  dd.curve.data=data.frame(Sites=dd.curve$sites, Richness=dd.curve$richness, SD=dd.curve$sd)
  dd.curve.data$label=rep(nm, nrow(dd.curve.data))
  dd.curve.data
}


pan_data=get_pan_data(dtm, nm="pan")
core_data=get_core_data(dt, label="core")

all = rbind(pan_data, core_data)

p = ggplot(all, aes(x=Sites, y=Richness, color=label))+
  geom_line()+
  geom_errorbar(aes(ymax = Richness + SD, ymin = Richness - SD), width = 0.25)+
  scale_color_brewer(palette = "Set1")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())