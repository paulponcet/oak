#' @title Conversion to a node
#' 
#' @description 
#' These methods convert an object to a node. 
#' A node is defined as an \code{rtree} object with no subtrees. 
#' 
#' @param x
#' An object to be converted. 
#' 
#' @param ...
#' Additional parameters. 
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
#' as.node(tr2) # the root of 'tr2'
#' 
#' ## Unrooted tree
#' (tr3 = r_(s = list(tr2, c_("Grand-Mother", "Father", "Son"))))
#' \dontrun{
#' as.node(tr3) # generates an error since 'tr3' is unrooted
#' }
#' 
as.node <- 
function(x, 
         ...)
{
  UseMethod("as.node")
}


# #' @export
# #' @rdname as.node
# #' 
# as.node.node <- 
# function(x, 
#          ...)
# {
#   x
# }


#' @export
#' @rdname as.node
#' 
as.node.character <- 
function(x, 
         ...)
{
  rtree(x[[1L]], subtrees = list(), ...)
}


#' @export
#' @rdname as.node
#' 
as.node.tree <- 
function(x, 
         ...)
{
  if (!is.rooted(x)) {
    stop("'x' is not a rooted tree, cannot convert it to a node")
  }
  root(x)
}
