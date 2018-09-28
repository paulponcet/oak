#' @title 
#' Test if an object is a node
#' 
#' @description 
#' The function \code{is.node} returns \code{TRUE} 
#' if \code{x} is a node, \code{FALSE} otherwise. 
#' A 'node' is just an \code{rtree} object with no subtrees. 
#' 
#' @param x
#' An object to be tested. 
#' 
#' @return 
#' A logical. 
#' 
#' @export
#' 
is.node <- 
function(x)
{
  is.rtree(x) && 
    #inherits(x, "node") && 
    length(subtrees(x))==0L
}
