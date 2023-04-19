tt_DEG_heatmap <- function(filter,i,j,tpm_matrix,meta_data,save_dir,lfcThreshold,
                           name="z-score",
                           show_column_names = TRUE,show_row_names=FALSE,
                           cluster_rows = TRUE,cluster_columns = FALSE,
                           heatmap_width=NULL,pdf_width=8,
                           name_suffix=NULL,color=NULL){
  if (is.null(color)){
    color=circlize::colorRamp2(c(-1.5,0,1.5),
                               colorRampPalette(c("#4875aa", "white", "#d7604c"))(3))
  }
  #heatmap
  tpm_filter <- tpm_matrix[rownames(filter),]

  #create annotaion
  cell_anno <- ComplexHeatmap::HeatmapAnnotation(cell=meta_data[colnames(tpm_filter),"stage"])

  filter$express <- NA
  filter[filter$log2FoldChange > 0,"express"] <- 'UP'
  filter[filter$log2FoldChange < 0,"express"] <- 'DOWN'

  gene_anno <- ComplexHeatmap::HeatmapAnnotation(express = filter[rownames(tpm_filter),"express"],which = 'row')
  heatmap_filter <- ComplexHeatmap::Heatmap(matrix = t(scale(t(tpm_filter))),name=name,
                                            show_column_names = show_column_names,
                                            show_row_names=show_row_names,
                                            cluster_rows = cluster_rows,
                                            cluster_columns = cluster_columns,
                                            top_annotation = cell_anno,
                                            left_annotation = gene_anno,
                                            width = heatmap_width,
                                            col=color)

  if (!file.exists(paste0(save_dir,"/filter_",lfcThreshold))){
    dir.create(paste0(save_dir,"/filter_",lfcThreshold))
  }

  pdf(paste0(save_dir,"/filter_",lfcThreshold,"/heatmap_",i,"_",j,name_suffix,".pdf"),width = pdf_width)
  print(heatmap_filter)
  dev.off()
}
