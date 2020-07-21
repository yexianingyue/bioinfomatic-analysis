library(dplyr)
library(plyr)
library(reshape2)


## 
get_effect_size = function(Microbe_Data, Other_Data){
  require(dplyr)
  microbe_name_order = data.frame(names=row.names(Microbe_Data), rank=1:nrow(Microbe_Data)) ## Get order
  out = list()
  for (i in 1:nrow(Other_Data)){
    OTU_data = data.frame(names=colnames(Other_Data), value=as.numeric(Other_Data[i,]))
    microbe_name_order_mg = merge(microbe_name_order, OTU_data, by = "names") %>% arrange(rank)
    #print(microbe_name_order_mg)
    a = adonis(Microbe_Data~microbe_name_order_mg[,3])
    ar = a$aov.tab$R2[1]
    ap = a$aov.tab$`Pr(>F)`[1]
    otu_name = rownames(Other_Data)[i]
    out[otu_name] = list(c(ar, ap))
  }
  out
}

corr_sig <- function(sig_vout, sig_data_corr){
  corr_out = list()
  sig_vout[,1] = as.character(sig_vout[,1]); sig_vout[,2] = as.character(sig_vout[,2])
  set='' ; set[1] = sig_vout[1,1]
  for (i in 2:nrow(sig_vout)){
    for (j in 1:length(set)){
      query = paste(sig_vout[i,1], set[j], sep = "")
      sig_data_corr$subject = paste(sig_data_corr[,1], sig_data_corr[,2], sep="")
      corr_coef = subset(sig_data_corr, subject == query)$value
      #print(query)
      swich = 'on'
      if (corr_coef  >0.5){
        corr_info = c(set[j], sig_vout[i,1], which(sig_vout[,1] == set[j]) , i, corr_coef)
        print(paste(corr_info, collapse = ' '))
        corr_out[length(corr_out)+1] = list(corr_info)
        swich = 'off'
        break
      }
    }
    if (swich == 'on'){
      set[length(set)+1] = sig_vout[i,1]
    }
  }
  return(list(cor_filt = set, filt_info = corr_out))
}



# calculate effect size b to a  (a is main body, b is intervention factors)
effect_size_analysis <- function(a, b){
  pacman::p_load(fdrtool, vegan, reshape2, plyr, dplyr)
  a = t(a) %>% as.data.frame()
  ## step1 calculate effect size for each item of b.
  adonis = get_effect_size(a, b) %>% ldply()
  adonis$qvalue = fdrtool(adonis$V2, statistic="pvalue")$qval
  sig_vout = subset(adonis, qvalue < 0.1) %>% arrange(-V1)  # qvalue < 0.01
  sig_data = b[row.names(b) %in% sig_vout$.id, ] # subsample the sig data
  #return(adonis)
  sig_data_corr = WGCNA::cor(t(sig_data), nThreads=12) %>% melt() # Note the number of threads...
  ## step2 rid of the items with strong correlation (Only retain the one with higher effect size). 
  #return(list(votu = sig_vout, corr = sig_data_corr))
  cor_filt = corr_sig(sig_vout, sig_data_corr)
  ## Order the data
  # get the order of a
  ordered_a = data.frame(names=row.names(a), rank=1:nrow(a)) 
  # order the b
  sig_data_cor_filt = t(sig_data) %>% as.data.frame() %>% subset(select = cor_filt$cor_filt)
  combined_ordered_data = merge(ordered_a, sig_data_cor_filt, by.x='names', by.y='row.names') %>% arrange(rank)
  rownames(combined_ordered_data) = combined_ordered_data$names
  combined_ordered_data =combined_ordered_data[, 3:ncol(combined_ordered_data)] 
  ## step3  calculate effect size for combined data
  comb_sig = adonis(a~. , data = combined_ordered_data )
  ## step4 calculate effect RsquareAdj for combined data
  adjr = RsquareAdj(1-comb_sig$aov.tab$R2[length(comb_sig$aov.tab$R2)-1], 60, length(comb_sig$aov.tab$R2)-2)
  
  result = list(adjr = adjr, sig_vout = sig_vout, cor_filt = cor_filt, combined_ordered_data = combined_ordered_data)
  return(result)
}

##
setwd('./network/')
dnadata = read.table("DNA_virus_abundance.txt", header = T,  sep = "\t", row.names = 1) 
#rnadata = read.table("RNA_Species_abundance.txt", header = T,  sep = "\t", row.names = 1)
microbe = read.table("metaphlan2.abundance", header = T, row.names = 1,sep = "\t")

##
colnames(dnadata) = gsub("D","", colnames(dnadata))
colnames(rnadata) = gsub("R","", colnames(rnadata))
dnadata = dnadata[rowSums(dnadata)>0, ]


#virome_data = rbind(dnadata, rnadata)
#tax
microbe_tax = read.table("metaphlan2.tax", header = T,sep = "\t")

## 
mic_to_dna = effect_size_analysis(dnadata, microbe)



#ls -d wkdir/* | rush -j 2  'cat {}/2.polished/.//07.kmer_count/*polish.ref.sh.work/polish_genome*/genome.nextpolish.part*.fasta > {}/2.polished/{%}.polished.fa'