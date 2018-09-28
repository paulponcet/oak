#' @title 
#' Reverse a chain
#' 
#' @description 
#' This function reverses the order of the nodes 
#' in a chain. 
#' 
#' @param x
#' A chain to be reversed. 
#' 
#' @return 
#' The reversed chain. 
#' 
#' @export
#' 
#' @examples 
#' (tr0 = c_("Bob", "Carl", "Daniel"))
#' (rev(tr0))
#' 
rev.rtree <- 
function(x)
{
  if (!is.chain(x)) {
    stop("'x' is not a chain, can't reverse it")
  }
  l = flatten(x)
  chain(rev(l))
}
