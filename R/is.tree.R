#' @title 
#' Test if an object is a tree
#' 
#' @description 
#' The function \code{is.tree} returns \code{TRUE} 
#' if \code{x} is a tree, \code{FALSE} otherwise. 
#' 
#' @param x
#' An object to be tested. 
#' 
#' @return 
#' A logical. 
#' 
#' @export
#' 
#' @examples
#' ## FALSE
#' is.tree(empty_tree())
#' 
is.tree <-
function(x)
{
  is.rtree(x) #|| is.gtree(x)  
}


#' @export
#' @rdname is.tree
#' 
is.rtree <- 
function(x)
{
  l = attr(x, "label")
  test1 = is.list(x) && 
    (is.null(l) || is.character(l)) && 
    inherits(x, "rtree") &&
    inherits(x, "tree") &&
    !anyDuplicated(labels(x))
  if (!test1) {
    return(FALSE)
  }
  test2 = all(sapply(x, FUN = is.rtree))
  test2
}
