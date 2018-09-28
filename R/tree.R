#' @title 
#' Tree constructor
#' 
#' @description 
#' The function \code{rtree} creates an 'rtree' (recursive tree) object. 
# The function \code{gtree} creates a 'gtree' (graph tree) object. 
#' 
#' @param label
#' character. The label of the root of the tree created. 
#' If \code{label=NULL}, the tree created is unrooted. 
#' If \code{label=NULL} and \code{subtrees=list()}, the tree 
#' created is the empty tree. 
#' 
#' @param subtrees
#' A (possibly empty) list of \code{rtree} objects. 
#' These rtrees must be rooted, otherwise an error is thrown. 
#' 
#' @param ...
#' Additional arguments to be passed as attributes to each 
#' node of the tree. 
#' 
#' @param x
#' A tree.  
#' 
#' @return
#' An \code{rtree} object. 
#' 
#' @export
#' 
#' @examples 
#' ## Chains
#' (tr0 = c_("Bob", "Carl", "Daniel"))
#' (tr1 = c_("Bill", "Caroline", "Dimitri", "Enoc"))
#' 
#' ## Rooted tree
#' (tr2 = r_("Alice", s = list(tr0, tr1)))
#' 
#' ## Unrooted tree
#' (tr3 = r_(s = list(tr2, c_("Grand-Mother", "Father", "Son"))))
#'              
rtree <- 
function(label = NULL, 
         subtrees = list(), 
         ...)
{
  properties = list(...)
  if (!is.list(subtrees) || 
      !all(sapply(subtrees,
                  function(x) { is.rtree(x) && is.rooted(x) }))) {
    stop("incorrect 'subtrees', must be a list of rooted 'rtrees'")
  }
  tr = subtrees
  attributes(tr) = properties
  if (!is.null(label)) {
    attr(tr, "label") = as.character(label)
  }
  class(tr) = c("rtree", "tree")
  if (!is.rtree(tr)) {
    stop("incorrect arguments, labels must be unique")
  }
  tr
}

#' @export
#' @rdname rtree
#' 
r_ <- rtree


#' @export
#' @rdname rtree
#' 
empty_tree <- 
function()
{
  rtree(NULL, list())
}


#' @importFrom bazar is.empty
#' @export
#' @rdname rtree
#' 
is.empty.rtree <- 
function(x)
{
  identical(x, empty_tree())
}
