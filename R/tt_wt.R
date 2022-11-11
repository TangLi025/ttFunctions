#' tt's write.table()
#'
#' @param x
#' @param path
#' @param row.names
#' @param col.names
#' @param sep
#' @param quote
#'
#' @param
#' @return
#' @export
#'
#' @examples
tt_wt <- function(x, path,row.names=F,col.names=F,sep='\t',quote=F) {
  write.table(x, path,row.names=row.names,col.names=col.names,sep=sep,quote=quote)
}
