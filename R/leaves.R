#' @title 
#' Leaves of a tree
#' 
#' @description 
#' The function \code{leaves} returns the leaves  
#' of a tree. 
#' 
#' @param .tree
#' A tree. 
#' 
#' @param .node
#' A node of \code{.tree}. 
#'  
#' @return 
#' A (possibly empty) list of nodes. 
#' 
#' @export
#' 
#' @examples 
#' ## Chains
#' (tr0 = c_("Bob", "Carl", "Daniel"))
#' leaves(tr0)
#' (tr1 = c_("Bill", "Caroline", "Dimitri", "Enoc"))
#' leaves(tr1)
#'
#' ## Rooted tree
#' (tr2 = r_("Alice", s = list(tr0, tr1)))
#' leaves(tr2)
#' 
#' ## Unrooted tree
#' (tr3 = r_(s = list(tr2, c_("Grand-Mother", "Father", "Son"))))
#' leaves(tr3)
#' 
leaves <- # :: Tree -> [Node]
function(.tree)
{
  UseMethod("leaves")
}


#' @importFrom foreach foreach 
#' @importFrom foreach %do%
#' @export
#' @rdname leaves
#' 
leaves.rtree <- # :: Tree -> [Node]
function(.tree)
{
  if (is.empty(.tree)) {
    return(list())
  }
  if (length(.tree)==0L) {
    return(list(root(.tree)))
  }
  i = 1L
  foreach::foreach(i = seq_along(.tree), .combine = 'c') %do% {
    leaves(.tree[[i]])
  }
}



#' @export
#' @rdname leaves
#' 
is_leafnode <- 
function(.node, 
         .tree)
{
  if (is.node(.node)) {
    .node = label(.node)  
  }
  ls = leaves(.tree)
  .node %in% sapply(ls, FUN = label)
}


#' @title 
#' Cut the leaves of a tree
#' 
#' @description 
#' The function \code{cut_leaves} cuts the leaves 
#' in \code{.tree}. 
#' 
#' @param .tree
#' A tree. 
#' 
# #' @param .node
# #' A node of \code{.tree} under which to cut the leaves. 
# #' 
#' @return 
#' A tree. 
#' 
#' @export
#' 
cut_leaves <- 
function(.tree)
{
  if (length(.tree) == 0L) {
    return(empty_tree())
  }
  h = sapply(.tree, FUN = height) # TODO: couteux a calculer, dommage de le recalculer a chaque appel recursif...
  .tree = cut_leaves1(.tree, h)
  if (length(.tree) == 0L) {
    .tree
  } else {
    sub = lapply(.tree, FUN = cut_leaves)
    update(.tree, sub)
  }
}


cut_leaves1 <- 
function(.tree, 
         heights)
{
  w = which(heights > 1L)
  update(.tree, as.list(.tree)[w])
}
