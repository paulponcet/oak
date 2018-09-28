#' @title 
#' Test if a tree is a chain
#' 
#' @description 
#' The function \code{is.chain} returns \code{TRUE} 
#' if \code{x} is a chain, \code{FALSE} otherwise. 
#' 
#' @param x
#' A tree to be tested. 
#' 
#' @return 
#' A logical. 
#' 
#' @export
#' 
#' @examples
#' ## FALSE
#' is.chain(empty_tree())
#' 
#' ## TRUE
#' (tr0 = c_("Bob", "Carl", "Daniel"))
#' is.chain(tr0)
#' 
#' ## FALSE
#' (tr1 = r_(s = list(tr0)))
#' is.chain(tr1)
#' 
#' ## FALSE
#' (tr = r_("titi", s = list(r_("toto"), r_("tata"))))
#' is.chain(tr)
#' 
is.chain <-
function(x)
{
  UseMethod("is.chain")
}


#' @export
#' @rdname is.chain
#' 
is.chain.rtree <-
function(x)
{
  if (!is.rtree(x) || !is.rooted(x) || length(x) > 1L) {
    return(FALSE)
  }
  if (length(x)==0L) {
    return(TRUE)
  }
  all(sapply(x, FUN = is.chain.rtree))
}
