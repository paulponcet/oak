#' @title 
#' Update a tree with new subtrees
#' 
#' @description 
#' Update a tree with new subtrees
#' 
#' @param object
#' A tree to be updated. 
#' 
#' @param subtrees
#' A list of trees. 
#' 
#' @param ...
#' Additional arguments to be passed as attributes to each 
#' node of the tree. 
#' 
#' @importFrom rlist list.merge
#' @export
#' 
update.rtree <- 
function(object, 
         subtrees = NULL, 
         ...)
{
  properties = list(...)
  if (length(properties) == 0L) {
    at = attributes(object)
  } else {
    at = rlist::list.merge(attributes(object), list(...))
  }
  if (!is.null(subtrees)) {
    object = subtrees
  }
  attributes(object) = at
  if (!is.tree(object)) {
    stop("incorrect 'subtrees' argument")
  }
  object
}
