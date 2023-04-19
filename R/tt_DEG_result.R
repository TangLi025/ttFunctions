tt_DEG_result <- function(dds,i,j,save_dir,name_suffix=NULL,independentFiltering = T){
  res_sample <- DESeq2::results(dds,contrast=c("stage",j,i),lfcThreshold = 0,alpha = 0.99,independentFiltering=independentFiltering)
  summary(res_sample)

  write.table(res_sample,paste0(save_dir,"/res_",i,"_",j,name_suffix,".txt"),sep='\t',quote = F,row.names = T)
  return(res_sample)
}
