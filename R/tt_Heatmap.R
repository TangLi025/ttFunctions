tt_Heatmap <- function(matrix,file_path,
                           name="z-score",
                           top_anno=NULL,
                           left_anno=NULL,
                           show_column_names = TRUE,show_row_names=FALSE,
                           cluster_rows = TRUE,cluster_columns = FALSE,
                           heatmap_width=NULL,pdf_width=NULL,                           
                           color=NULL){
  if (is.null(color)){
    color=circlize::colorRamp2(c(-1.5,0,1.5),
                               colorRampPalette(c("#4875aa", "white", "#d7604c"))(3))
  }

  heatmap <- ComplexHeatmap::Heatmap(matrix = na.omit(t(scale(t(matrix)))),name=name,
                                            show_column_names = show_column_names,
                                            show_row_names=show_row_names,
                                            cluster_rows = cluster_rows,
                                            cluster_columns = cluster_columns,
                                            top_annotation = top_anno,
                                            left_annotation = left_anno,
                                            width = heatmap_width,
                                            col=color)

  pdf(file_path,width = pdf_width)
  print(heatmap)
  dev.off()
}
