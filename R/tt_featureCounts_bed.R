
#use_package("Rsubread")
tt_featureCounts_bed <- function(fls,
                                    annot.ext,
                                    fracOverlap = 0.5,
                                    strandSpecific = 2,
                                 isPairedEnd=T,
                                    allowMultiOverlap=T,
                                 nthreads=50){
  fc_result <- Rsubread::featureCounts(files = fls,
              annot.ext = annot.ext,
              isGTFAnnotationFile = F,
              fracOverlap = fracOverlap,
              strandSpecific = strandSpecific,
              isPairedEnd=isPairedEnd,
              allowMultiOverlap=allowMultiOverlap,
              nthreads=nthreads)
  return(fc_result)
}
