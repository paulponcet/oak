#' @title 
#' Parent of a node
#' 
#' @description 
#' The function \code{parent} returns the parent 
#' of a node in a given tree. 
#' If the node is not found in the tree or has no parent, 
#' the empty tree is returned. 
#' 
#' @param .node
#' node or character. The node or node label considered. 
#'  
#' @param .tree
#' A tree. 
#' 
#' @param degree
#' integer. Currently not used. 
#' 
#' @return 
#' A node. 
#' 
#' @export
#' 
#' @examples 
#' ## Rooted tree
#' (tr0 = c_("Bob", "Carl", "Daniel"))
#' (tr1 = c_("Bill", "Caroline", "Dimitri", "Enoc"))
#' (tr2 = r_("Alice", s = list(tr0, tr1)))
#' 
#' ## Unrooted tree
#' (tr3 = r_(s = list(tr2, c_("Son", "Father", "Grand-Mother"))))
#' parent("Alice", tr3)
#' parent("Bob", tr3)
#' parent("any node", tr3)
#'
parent <- # :: Node -> Tree -> Node
function(.node, 
         .tree, 
         degree = 1L)
{
  UseMethod("parent", .tree)
}


#' @export
#' @rdname parent
#' 
parent.rtree <- # :: Node -> Tree -> Node
function(.node, 
         .tree, 
         degree = 1L) # TODO
{
  # if (missing(.node) || is.null(.node)) {
  #   stop("incorrect '.node' argument")
  # }
  if (is.node(.node)) {
    .node = label(.node)
  }
  if (is.root(.node, .tree)) {
    return(empty_tree())
  }
  x = sapply(.tree, FUN = label)
  if (.node %in% x) {
    return(root(.tree)) # BOF, cf. unrooted trees...
  }
  l = lapply(.tree, FUN = function(tr) parent(.node, tr))
  w = which(!sapply(l, FUN = is.empty))
  if (length(w) > 0) {
    as.node(l[[w]])
  } else {
    empty_tree()
  }
}


#' @export
#' @rdname parent
#' 
has.parent <- 
function(.node, 
         .tree)
{
  if (is.empty(.tree) || is.null(.node)) {
    return(FALSE)
  }
  if (is.node(.node)) {
    .node = label(.node)
  }
  r = roots(.tree)
  !.node %in% sapply(r, FUN = label) &&
    .node %in% labels(.tree)
}
