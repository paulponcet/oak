#' @title 
#' Add a tree at the bottom of a chain
#' 
#' @description 
#' The function \code{\%->\%} links the rooted tree \code{e2} 
#' to the bottom of the chain \code{e1}. 
#' 
#' @param e1
#' A chain. 
#' 
#' @param e2
#' A rooted tree. 
#' 
#' @return 
#' A rooted tree.  
#' 
#' @export
#' 
#' @examples 
#' ## Chain
#' (tr0 = c_("Bob", "Carl", "Daniel"))
#' 
#' ## Rooted tree
#' (tr1 = c_("Bill", "Caroline", "Dimitri", "Enoc"))
#' (tr2 = c_("John", "Thomas"))
#' (tr3 = r_("Alice", s = list(tr1, tr2))) 
#' 
#' ## Linking both
#' tr0 \%->\% tr3
#' 
"%->%" <-
function(e1, 
         e2)
{
  UseMethod("%->%")
}


#' @export
#' @rdname %->%
#' 
"%->%.rtree" <-
function(e1, 
         e2)
{
  if (is.character(e2)) {
    e2 = rtree(e2)
  }
  if (!is.chain(e1)) {
    stop("left hand side of %->% must be a chain")
  }
  if (!is.rtree(e2) || !is.rooted(e2)) {
    stop("right hand side of %->% must be a rooted tree")
  }
  if (is.node(e1)) {
    return(update(e1, sub = list(e2)))
  }
  n = leaves(e1)[[1]]
  n = update(n, sub = list(e2))
  cut_leaves(e1) %->% n
}


#' @export
#' @rdname %->%
#' 
"%->%.character" <-
function(e1, 
         e2)
{
  rtree(e1) %->% e2
}
