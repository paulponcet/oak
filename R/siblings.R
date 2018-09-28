#' @title 
#' Siblings of a node
#' 
#' @description 
#' The function \code{siblings} returns the siblings 
#' of a node in a given tree. 
#' 
#' @param .node
#' node or character. The node or label of the node considered. 
#'  
#' @param .tree
#' A tree. 
#' 
#' @param include_node
#' logical. If \code{FALSE} (the default), the node \code{.node} is 
#' not included to the list of siblings. 
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
#' siblings("Bob", tr2)
#' 
#' ## Unrooted tree
#' (tr3 = r_(s = list(tr2, c_("Grand-Mother", "Father", "Son"))))
#' siblings("Alice", tr3) # note that in 'tr3', Alice and Grand-Mother are not siblings
#' 
siblings <- # :: Node -> Tree -> [Node]
function(.node, 
         .tree, 
         include_node = FALSE)
{
  UseMethod("siblings", .tree)
}


#' @export 
#' @rdname siblings
#' 
siblings.rtree <- # :: Node -> Tree -> [Node]
function(.node, 
         .tree, 
         include_node = FALSE)
{
  if (is.node(.node)) {
    .node = label(.node)
  }
  pa = parent(.node, .tree)
  
  ## attention si le parent est NULL, les siblings n'en sont pas !!
  if (is.empty(pa)) {
    list()
    
  } else {
    ch = children(pa, .tree)
    if (include_node) {
      ch
    } else {
      x = sapply(ch, FUN = label)
      w = which(x==.node)
      ch[-w]
    }
  }
}
