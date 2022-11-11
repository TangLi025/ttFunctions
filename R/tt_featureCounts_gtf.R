
#use_package("Rsubread")
tt_featureCounts_gtf <- function(fls,
                                    annot.ext = "/disk/user_09/reference/annotation/mm39/raw/gencode.vM29.annotation_ERCC.gtf",
                                    GTF.featureType = "exon",
                                    GTF.attrType = "gene_id",
                                    GTF.attrType.extra = c("gene_name","gene_type"),
                                    fracOverlap = 0.5,
                                    strandSpecific = 2,
                                 isPairedEnd=T,
                                    allowMultiOverlap=T,
                                 nthreads=50){
  fc_result <- Rsubread::featureCounts(files = fls,
              annot.ext = annot.ext,
              isGTFAnnotationFile = T,
              GTF.featureType = GTF.featureType,
              GTF.attrType = GTF.attrType,
              GTF.attrType.extra = GTF.attrType.extra,
              fracOverlap = fracOverlap,
              strandSpecific = strandSpecific,
              isPairedEnd=isPairedEnd,
              allowMultiOverlap=allowMultiOverlap,
              nthreads=nthreads)
  return(fc_result)
}
