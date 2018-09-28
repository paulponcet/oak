#' @title 
#' Children of a node
#' 
#' @description 
#' The function \code{children} returns the children 
#' of a node in a given tree. 
#' 
#' @param .node
#' node or character. The node or node label considered. 
#'  
#' @param .tree
#' A tree. 
#' 
#' @param degree
#' integer. Currently not used. 
#' 
#' @return 
#' A (possibly empty) list of nodes. 
#' 
#' @export
#' 
children <- # :: Node -> Tree -> [Node]
function(.node, 
         .tree, 
         degree = 1L)
{
  UseMethod("children", .tree)
}    
    

#' @importFrom purrr flatten
#' @export
#' 
children.rtree <- # :: Node -> Tree -> [Node]
function(.node, 
         .tree, 
         degree = 1L) # TODO
{
  .tree = take_branch(.tree, .node)
  if (length(.tree) == 0L) {
    return(list())
  } else {
    chs = lapply(.tree, FUN = root)
    if (degree == 1L) {
      chs
    } else {
      unique(purrr::flatten(lapply(chs, FUN = children, .tree, degree = degree - 1L)))
    }
  }
}
