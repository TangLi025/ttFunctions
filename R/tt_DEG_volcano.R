tt_DEG_volcano <- function(res_sample,i,j,save_dir,
                           lfcThreshold,padj=0.05,padj_limit=20,
                           padj_label=10,lfc_label=10,
                          name_suffix=NULL,
                          pdf_width=NULL,pdf_height=NULL){

  #volcano_color <- c(UP = alpha("#C01623", 0.7),DOWN = alpha("#4431A5", 0.7),NC=alpha("#E5E1DD", 0.7))
  volcano_color <- c(UP = alpha("#CC0000", 0.7),DOWN = alpha("#2f5688", 0.7),NC=alpha("#BBBBBB", 0.7))

  res_sample$express <- ifelse(res_sample$log2FoldChange>lfcThreshold & res_sample$padj<padj,"UP",
                               ifelse(res_sample$log2FoldChange< -lfcThreshold & res_sample$padj<padj,"DOWN","NC"))
  res_up_gene <- res_sample[res_sample$express=="UP",]
  padj_label_up <- head(rownames(res_up_gene[order(res_up_gene$padj),]),padj_label)
  lfc_label_up <- head(rownames(res_up_gene[order(res_up_gene$log2FoldChange,decreasing =T),]),lfc_label)
  res_down_gene <- res_sample[res_sample$express=="DOWN",]
  padj_label_down <- head(rownames(res_down_gene[order(res_down_gene$padj),]),padj_label)
  lfc_label_down <- head(rownames(res_down_gene[order(res_down_gene$log2FoldChange,decreasing =F),]),lfc_label)
  label_genes <- c(padj_label_up,padj_label_down,lfc_label_up,lfc_label_down)
  label_genes <- label_genes[!duplicated(label_genes)]
  res_sample[label_genes,"volcano_label"] <- label_genes
  p <- ggplot2::ggplot(as.data.frame(res_sample))+
    ggplot2::geom_point(mapping=aes(x=log2FoldChange,y=-log10(padj),col=express),size=1,alpha=0.7)+
    #ggrepel::geom_text_repel(aes(x=log2FoldChange,y=-log10(padj),col=express,label=volcano_label),show.legend = F,position = position_nudge())+
    ggrepel::geom_text_repel(aes(x=log2FoldChange,y=-log10(padj),col=express,label=volcano_label),show.legend = F,max.overlaps=30)+
    ggplot2::scale_color_manual(values = volcano_color,
                       breaks=c("UP", "DOWN","NC"),
                       labels=c(paste0("UP (", nrow(res_sample[res_sample$express == "UP",]), ")"),
                                paste0("DOWN (",nrow(res_sample[res_sample$express == "DOWN",]),")"),
                                "NC"))+
    ggplot2::theme_bw()+
    ggplot2::ylim(0,padj_limit)+
    ggplot2::labs(x=expression(Log[2]~FoldChange),y=expression(-Log[10]~padj))+
    ggplot2::theme(legend.position = "right", #top
          legend.title = element_blank(),
          #legend.position = c(0.85,0.75),
          legend.background = element_blank(),
          panel.grid =element_blank(),
          panel.background = element_rect(fill = "white",colour="black",size=2),
          legend.key = element_blank(),
          legend.text = element_text(size = 15,  face = 'plain'),# bold
          legend.direction= "vertical")+ #horizontal,vertical
    ggplot2::theme(axis.text = element_text(size = 18), plot.margin = unit(c(0.5,0.5,0,0.5), "cm"))+
    ggplot2::theme(plot.margin = unit(c(0.5,0.5,0.5,0.3), "cm"))+ #调整与图片边缘的距离
    ggplot2::theme(axis.title.x = element_text(size = 18,margin = margin(t=8)))+
    ggplot2::theme(axis.title.y = element_text(size = 18,margin = margin(r=5 )))+
    ggplot2::ggtitle(paste0("DEG ",i," ",j))+
    ggplot2::theme(plot.title = element_text(hjust=0.5, size = 20,  face = 'bold'))+
    ggplot2::geom_hline(yintercept = -log10(padj),linetype="dashed") +
    ggplot2::geom_vline(xintercept = c(-lfcThreshold,lfcThreshold),linetype="dashed")
  if (!file.exists(paste0(save_dir,"/filter_",lfcThreshold))){
    dir.create(paste0(save_dir,"/filter_",lfcThreshold))
  }
  pdf(paste0(save_dir,"/filter_",lfcThreshold,"/volcano_",i,"_",j,name_suffix,".pdf"),width=pdf_width,height = pdf_height)
  print(p)
  dev.off()
}
