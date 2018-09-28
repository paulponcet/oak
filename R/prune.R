#' @title 
#' Prune a tree
#' 
#' @description 
#' The function \code{prune} removes nodes in a tree 
#' whose height is greater than a given threshold. 
#' 
#' @param .tree
#' A tree to be pruned. 
#' 
#' @param max_height
#' integer. The height imposed to the new tree. 
#' 
#' @return 
#' The pruned tree. 
#' 
#' @export
#' 
prune <- # :: Tree -> Tree
function(.tree,
         max_height = 1L)
{
  UseMethod("prune")  
}


#' @export
#' @rdname prune
#' 
prune.rtree <- # :: Tree -> Tree
function(.tree,
         max_height = 1L)
{
  #if (missing(.node)) {
  #  .node = label(.tree)
  #}
  if (max_height == 0L) {
    return(empty_tree())
  }
  if (length(.tree) == 0L) {
    sub = NULL
  } else if (max_height == 1L && is.rooted(.tree)) {
    sub = list()
  } else {
    i = is.rooted(.tree)
    sub = lapply(.tree, FUN = function(x) prune(x, max_height = max_height - i))
  }
  update(.tree, sub)
}
