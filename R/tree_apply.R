#' @title 
#' Apply a function to each node of a tree
#' 
#' @description 
#' \code{tree_apply} applies a function \code{fun} 
#' to each node of \code{.tree} and stores the results 
#' in the attribute \code{at}. 
#' 
#' @param .tree
#' A tree. 
#' 
#' @param at
#' character. Name of the attribute to be created at each node of 
#' \code{.tree} that will contain the result of \code{fun}. 
#' 
#' @param fun
#' function or character. A function taking two arguments \code{.node} and \code{.tree} 
#' (in this order), to be applied to each node of the tree. 
#' 
#' @param ...
#' Additional arguments to be passed to \code{fun}. 
#' 
#' @importFrom bazar as.fun
#' @export
#' 
#' @examples 
#' ## Rooted tree
#' (tr0 = c_("Bob", "Carl", "Daniel"))
#' (tr1 = c_("Bill", "Caroline", "Dimitri", "Enoc"))
#' (tr2 = r_("Alice", s = list(tr0, tr1)))
#' 
#' ## Unrooted tree
#' (tr3 = r_(s = list(tr2, c_("Grand-Mother", "Father", "Son"))))
#' 
#' f <- function(.node, .tree) nchar(label(.node))
#' tr4 = tree_apply(tr3, at = "value", fun = f)
#' print(tr4, at = "value")
#' 
#' g <- function(.node, .tree) height(take_branch(.tree, .node))
#' tr5 = tree_apply(tr3, at = "height", fun = g)
#' print(tr5, at = "height")
#' 
tree_apply <- 
function(.tree, 
         ...)
{
  UseMethod("tree_apply")  
}


#' @export
#' @rdname tree_apply
#' 
tree_apply.rtree <- 
function(.tree, # :: Tree -> Tree
         at, 
         fun, # FUN :: Tree -> a
         ...)
{
  if (is.empty(.tree)) {
    return(.tree)
  }
  fun = bazar::as.fun(fun)
  if (is.rooted(.tree)) {
    f = fun(root(.tree), .tree, ...)
    attr(.tree, at) = f
  }
  sub = lapply(.tree, FUN = tree_apply, at = at, fun = fun, ...)
  update(.tree, sub)
}
