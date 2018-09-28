#' @title 
#' Ancestors of a node
#' 
#' @description 
#' The function \code{ancestors} returns the ancestors
#' of a node in a given tree. 
#' 
#' @param .node
#' node or character. The node or node label considered. 
#'  
#' @param .tree
#' A tree. 
#' 
#' @param include_node
#' logical. If \code{FALSE} (the default), \code{.node} is not part 
#' of the list returned. 
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
#' ancestors("Alice", tr2)
#' ancestors("Daniel", tr2, include_node = TRUE)
#' 
#' ## Unrooted tree
#' (tr3 = r_(s = list(tr2, c_("Grand-Mother", "Father", "Son"))))
#' ancestors("Alice", tr3)
#' ancestors("Alice", tr3, include_node = TRUE)
#' ancestors("Daniel", tr3)
#' ancestors("Son", tr3)
#' ancestors("Son", tr3, include_node = TRUE)
#' 
ancestors <- # :: Node -> Tree -> [Node]
function(.node, 
         .tree, 
         include_node = FALSE)
{
  UseMethod("ancestors", .tree)
} 


#' @export
#' @rdname ancestors
#' 
ancestors.rtree <- # :: Node -> Tree -> [Node]
function(.node, 
         .tree, 
         include_node = FALSE)
{
  if (is.node(.node)) {
    .node = label(.node)
  }
  i = is.root(.node, .tree)
  if (i && !include_node) {
    return(list())
  } else if (i && include_node) {
    return(list(.node))
  }
  p = parent(.node, .tree)
  ## If '.node' does not exist in '.tree': 
  if (is.empty(p)) {
    return(list())
  }
  as = c(list(p), ancestors(label(p), .tree, include_node = FALSE))
  if (include_node) {
    n = take_node(.node, .tree)
    c(list(n), as)
  } else {
    as
  }
}
