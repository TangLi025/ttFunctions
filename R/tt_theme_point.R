tt_theme_point <- function(xlab="sample1",ylab="sample2",legend.position="top",legend.title = element_blank()){
    volcano_color <- c("#C01623","DimGray","#4431A5")
    p1 <- theme(legend.position = legend.position,
          legend.title = legend.title,
          legend.background = element_blank(),
          panel.grid =element_blank(),
          panel.background = element_rect(fill = "white",colour="black",size=2),
          legend.key = element_blank(),
          legend.text = element_text(size = 15,  face = 'bold'),
          legend.direction= "vertical",
          axis.text = element_text(size = 18), plot.margin = unit(c(0.5,0.5,0,0.5), "cm"),plot.margin = unit(c(0.5,0.5,0.5,0.3), "cm"), #调整与图片边缘的距离
          axis.title.x = element_text(size = 18,margin = margin(t=8)),
          axis.title.y = element_text(size = 18,margin = margin(r=5 )))
  return(p1)
}