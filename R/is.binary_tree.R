#' @title 
#' Test if a tree is a binary tree
#' 
#' @description 
#' The function \code{is.binary_tree} tests if 
#' a tree is binary. 
#' 
#' @param x
#' A tree to be tested. 
#' 
#' @return 
#' A logical. 
#' 
#' @export
#' 
#' @examples 
#' ## FALSE
#' is.binary_tree(empty_tree())
#' 
#' ## FALSE
#' tr = r_(s = list(r_("toto"), r_("tata")))
#' is.binary_tree(tr) # unrooted tree
#' 
#' ## TRUE
#' tr = r_("titi", s = list(r_("toto"), r_("tata")))
#' is.binary_tree(tr)
#' 
is.binary_tree <- 
function(x)
{
  if (!is.tree(x) || !is.rooted(x)) {
    return(FALSE)
  }
  e = subtrees(x)
  if (length(e) == 0L) {
    return(TRUE)
  }
  if (length(e) != 2L) {
    return(FALSE)
  }
  all(sapply(e, FUN = is.binary_tree))
}
