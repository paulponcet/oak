#' @title Labels of nodes and trees
#' 
#' @description 
#' Get the label of a node or the labels of all nodes 
#' of a tree. 
#' 
#' @param .node,x
#' A node. 
#' 
#' @param .tree,object
#' A tree. 
#' 
#' @param value
#' character. New label to be applied to the node.  
#' 
#' @param ...
#' Additional arguments (not used). 
#' 
#' @export
#' 
#' @examples 
#' ## Rooted tree
#' (tr0 = c_("Bob", "Carl", "Daniel"))
#' (tr1 = c_("Bill", "Caroline", "Dimitri", "Enoc"))
#' (tr2 = r_("Alice", s = list(tr0, tr1)))
#' labels(tr0)
#' labels(tr1)
#' labels(tr2)
#' 
#' ## Unrooted tree
#' (tr3 = r_(s = list(tr2, c_("Grand-Mother", "Father", "Son"))))
#' labels(tr3)
#' 
label <-
function(.node)
{
  attr(.node, "label")
}


#' @export
#' @rdname label
#' 
"label<-" <-
function(x, 
         value)
{
  attr(x, "label") = value
  x
}


#' @importFrom foreach foreach %do%
#' @export
#' @rdname label
#' 
labels.rtree <- 
function(object, 
         ...)
{
  l = label(object)
  if (length(object) == 0L) {
    return(l)
  }
  i = 1L
  r = foreach::foreach(i = seq_along(object), .combine = 'c') %do% {
    labels(object[[i]], ...)
  }
  c(l, r)
}
