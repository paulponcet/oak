#' @title 
#' Flatten a tree
#' 
#' @description 
#' The function \code{flatten} returns all the 
#' nodes that compose a given tree. 
#' 
#' @param .tree
#' A tree to be flattened. 
#' 
#' @return 
#' A (possibly empty) list of nodes. 
#' 
#' @export
#' 
#' @examples 
#' ## Rooted tree
#' (tr0 = c_("Bob", "Carl", "Daniel"))
#' (tr1 = c_("Bill", "Caroline", "Dimitri", "Enoc"))
#' (tr2 = r_("Alice", s = list(tr0, tr1)))
#' flatten(tr2)
#' 
#' ## Unrooted tree
#' (tr3 = r_(s = list(tr2, c_("Grand-Mother", "Father", "Son"))))
#' flatten(tr3)
#' 
flatten <- 
function(.tree)
{
  UseMethod("flatten")
}


#' @importFrom foreach foreach 
#' @importFrom foreach %do%
#' @export
#' @rdname flatten
#' 
flatten.rtree <- # :: Tree -> [Node]
function(.tree)
{
  if (is.empty(.tree)) {
    return(list())
  }
  if (length(.tree) == 0L) {
    return(list(root(.tree)))
  }
  i = 1L
  r = foreach::foreach(i = seq_along(.tree), .combine = 'c') %do% {
    flatten(.tree[[i]])
  }
  if (is.rooted(.tree)) {
    c(roots(.tree), r)
  } else {
    r
  }
}
