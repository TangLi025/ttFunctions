tt_enrichGO <- function(genelist,filedir,groupname,OrgDb = org.Mm.eg.db::org.Mm.eg.db,ont = "all",
                    pAdjustMethod = "BH",pvalueCutoff = 0.05,qvalueCutoff = 0.1,
                    height=NULL,width=NULL){
  df <- clusterProfiler::bitr( genelist, 
              fromType = "SYMBOL", toType = c( "ENTREZID"), OrgDb = OrgDb ) # 可以换成人的
  ego <- clusterProfiler::enrichGO(
    gene = df$ENTREZID,
    keyType = "ENTREZID",
    OrgDb = OrgDb,
    ont = ont, # "BP", "MF", "CC"
    pAdjustMethod = pAdjustMethod,
    pvalueCutoff = pvalueCutoff,
    qvalueCutoff = qvalueCutoff,
    readable = TRUE
  )
  
  if (nrow(as.data.frame(ego))==0){
    return(ego)
  }
  
  # significant
  ego_df <- as.data.frame(ego)
  dat <- ego_df[ego_df$p.adjust < qvalueCutoff, ]
  dat <- dat[order(dat$p.adjust, decreasing = F), ]
  all_GO=dat
  write.csv(all_GO, paste0(filedir,"/",groupname,"_GOenrich_significant.csv"), row.names = F)
  
  go_dot <- enrichplot::dotplot(ego, orderBy = "x", showCategory =10,split="ONTOLOGY",
                    title = paste0("GO_",groupname)) + ggplot2::facet_grid(ONTOLOGY~., scale='free')
  
  pdf(file=paste0(filedir,"/Go_dot_",groupname,".pdf"),height = height,width = width)
  print(go_dot)
  dev.off()
  return(all_GO)
}