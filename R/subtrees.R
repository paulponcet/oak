#' @title Subtrees of a tree
#' 
#' @description 
#' The function \code{subtrees} returns the list of 
#' subtrees of the root of a given tree. 
#' 
#' @param .tree,x
#' A tree. 
#' 
#' @param value
#' Subtrees to be assigned to \code{x}. 
#' 
#' @return 
#' A (possibly empty) list of trees. 
#' 
#' @export
#' 
#' @examples 
#' ## Rooted tree
#' (tr0 = c_("Bob", "Carl", "Daniel"))
#' (tr1 = c_("Bill", "Caroline", "Dimitri", "Enoc"))
#' (tr2 = r_("Alice", s = list(tr0, tr1)))
#' (subtrees(tr2))
#' 
#' ## Unrooted tree
#' (tr3 = r_(s = list(tr2, c_("Son", "Father", "Grand-Mother"))))
#' (subtrees(tr3))
#' 
subtrees <-
function(.tree)
{
  UseMethod("subtrees")
}

#' @export
#' @rdname subtrees
#' 
subtrees.rtree <- 
function(.tree)
{
  as.list(.tree, recursively = FALSE)
}


#' @export
#' @rdname subtrees
#' 
"subtrees<-" <-
function(x,    # tree
         value)# list of trees
{
  attributes(x) = attributes(value)
  x
}
