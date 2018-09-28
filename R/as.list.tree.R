#' @title Conversion of a tree to a list
#' 
#' @description 
#' This function converts a tree to a list. 
#' 
#' @param x
#' A tree. 
#' 
#' @param recursively
#' logical. See below. 
#' 
#' @param ...
#' Additional arguments (not used). 
#' 
#' @return 
#' A list. 
#' If \code{recursively=FALSE}, this list is made up of the subtrees of \code{x}. 
#' If \code{recursively=TRUE}, these subtrees are themselves recursively 
#' converted to a list. 
#' 
#' @export
#' 
#' @examples 
#' (tr0 = c_("Bob", "Carl", "Daniel"))
#' (tr1 = c_("Bill", "Caroline", "Dimitri", "Enoc"))
#' (tr2 = r_("Alice", s = list(tr0, tr1)))
#' as.list(tr2)
#' as.list(tr2, rec = TRUE)
#' 
#' ## Unrooted tree
#' (tr3 = r_(s = list(tr2, c_("Grand-Mother", "Father", "Son"))))
#' as.list(tr3)
#' as.list(tr3, rec = TRUE)
#' 
as.list.rtree <- 
function(x, 
         recursively = FALSE, 
         ...)
{
  class(x) = "list"
  #attributes(x) = NULL
  if (length(x) == 0L || !recursively) {
    return(x)
  }
  l = lapply(x, FUN = as.list.rtree, recursively = recursively, ...)
  attributes(l) = attributes(x)
  l
}
