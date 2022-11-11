tt_enrichKEGG <- function(genelist,filedir,groupname,OrgDb = org.Mm.eg.db::org.Mm.eg.db,
                      pAdjustMethod = "BH",pvalueCutoff = 0.05,qvalueCutoff = 0.1,
                      organism = 'mmu',
                    height=NULL,width=NULL){
  df <- clusterProfiler::bitr( genelist, 
              fromType = "SYMBOL", toType = c( "ENTREZID"), OrgDb = OrgDb )
  KEGG=clusterProfiler::enrichKEGG(gene=df$ENTREZID, organism = organism, keyType = 'kegg', pvalueCutoff = pvalueCutoff,pAdjustMethod = pAdjustMethod, 
                  minGSSize = 10,maxGSSize = 500,qvalueCutoff = qvalueCutoff,use_internal_data = T)
  if (nrow(as.data.frame(KEGG))==0){
    return(KEGG)
  }
  write.csv(summary(KEGG),paste0(filedir,"/", groupname,"_KEGGenrich_significant.csv"),row.names =F)
  
  KEGG_dot <- enrichplot::dotplot(KEGG,title=paste0("KEGG_",groupname),showCategory = 10) 
  pdf(file=paste0(filedir,"/",groupname,".pdf"), width=width, height=height)
  print(KEGG_dot)
  dev.off()
  return(KEGG)
}