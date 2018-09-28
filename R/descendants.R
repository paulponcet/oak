#' @title Descendants of a node
#' 
#' @description 
#' The function \code{descendants} returns the descendants 
#' of a node in a given tree. 
#' 
#' @param .node
#' node or character. The node or node label considered. 
#'  
#' @param .tree
#' A tree. 
#' 
#' @param include_node
#' logical. Currently not used. 
#' 
#' @return 
#' A (possibly empty) list of nodes. 
#' 
#' @export
#' 
descendants <-
function(.node, 
         .tree, 
         include_node = FALSE)
{
  UseMethod("descendants", .tree)
} 


# #' @importFrom foreach foreach 
# #' @importFrom foreach %do%
#' @export
#' @rdname descendants
#' 
descendants.rtree <-
function(.node, 
         .tree, 
         include_node = FALSE) # TODO
{
  .tree = take_branch(.tree, .node)
  f = flatten(.tree)
  if (is.rooted(.tree)) {
    f[-1L]
  } else {
    f
  }
  #if (missing(.node)) {
  #  .node = label(.tree)
  #}
  #if (is.node(.node)) {
  #  .node = label(.node)
  #}
  #.tree = find_branch(.tree, .node)
  #if (length(.tree)==0L) {
  #  return(list())
  #}
  #r = foreach::foreach(i=seq_along(.tree), .combine = 'c') %do% {
  #  descendants(.tree[[i]])
  #}
  #return(c(children(.tree), r))
}
