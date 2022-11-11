tt_DEG_filter <- function(res_sample,i,j,lfcThreshold,padj=0.05,name_suffix=NULL){
  res_sample<- res_sample[!is.na(res_sample$padj) & !is.na(res_sample$log2FoldChange),]
  summary(res_sample)

  filter <- res_sample[abs(res_sample$log2FoldChange) > lfcThreshold & res_sample$padj < padj,]
  summary(filter)
  if (!file.exists(paste0("02_DEG_result/filter_",lfcThreshold))){
    dir.create(paste0("02_DEG_result/filter_",lfcThreshold))
  }

  write.table(filter,paste0("02_DEG_result/filter_",lfcThreshold,"/","res_filter_",i,"_",j,name_suffix,".txt"),quote = F,row.names = T,col.names = T)

  up_genename <- rownames(filter[filter$log2FoldChange>0,])
  write.table(up_genename,
              paste0("02_DEG_result/filter_",lfcThreshold,"/","res_",i,"_",j,name_suffix,"up_genename.txt"),
              sep="\t",quote = F,row.names = F,col.names = F)

  down_genename <- rownames(filter[filter$log2FoldChange<0,])
  write.table(down_genename,
              paste0("02_DEG_result/filter_",lfcThreshold,"/","res_",i,"_",j,name_suffix,"down_genename.txt"),
              sep="\t",quote = F,row.names = F,col.names = F)
  return(filter)
}
