#' @title 
#' Height of a tree
#' 
#' @description 
#' This function returns the height of a tree, 
#' defined as the number of nodes of the longest subchain. 
#' For an empty tree, the height is conventionally equal to 0. 
#' 
#' @param .tree,x
#' A tree. 
#' 
#' @param value
#' integer. Height to assign to the tree \code{x} 
#' (this calls the function \code{\link[oak]{prune}}). 
#' 
#' @return 
#' An integer. 
#' 
#' @export
#' 
#' @examples 
#' ## Rooted tree
#' (tr0 = c_("Bob", "Carl", "Daniel"))
#' tr1 = r_("Dimitri", s = list(c_("Enoc"), c_("Ferdinand")))
#' tr1 = r_("Caroline", s = list(tr1))
#' (tr1 = r_("Bill", s = list(tr1)))
#' (tr2 = r_("Alice", s = list(tr0, tr1)))
#' height(empty_tree())
#' height(tr0)
#' height(tr1)
#' height(tr2)
#' 
#' ## Unrooted tree
#' (tr3 = r_(s = list(tr2, c_("Grand-Mother", "Father", "Son"))))
#' height(tr3)
#' 
height <- # :: Tree -> Int
function(.tree)
{
  UseMethod("height")
}
  

#' @export
#' @rdname height
#' 
height.rtree <- # :: Tree -> Int
function(.tree)
{
  if (identical(.tree, empty_tree())) {
    return(0L)
  }
  if (length(.tree)==0L) {
    return(1L)
  }
  k = ifelse(is.rooted(.tree), 1L, 0L)
  max(sapply(.tree, FUN = height.rtree)) + k
}


#' @export
#' @rdname height
#' 
"height<-" <- 
function(x, 
         value)
{
  prune(x, max_height = value)
}
