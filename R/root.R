#' @title 
#' Test if a node is a root of a tree
#' 
#' @description 
#' The function \code{is.root} returns \code{TRUE} 
#' if \code{.node} is a root of \code{tree}, \code{FALSE} otherwise. 
#' 
#' @param .node
#' A node or node label to be tested. 
#' 
#' @param .tree
#' A tree
#' 
#' @return 
#' A logical. 
#' 
#' @export
#' 
is.root <-
function(.node, 
         .tree)
{
  if (is.node(.node)) {
    .node = label(.node)
  }
  r = roots(.tree)
  !is.null(.node) && .node %in% sapply(r, FUN = label)
}


#' @title 
#' Test if a tree has a root
#' 
#' @description 
#' The function \code{is.rooted} returns \code{TRUE} 
#' if \code{.tree} is a rooted tree, \code{FALSE} otherwise. 
#' 
#' @param .tree
#' A tree to be tested. 
#' 
#' @return 
#' A logical. 
#' 
#' @export
#' 
is.rooted <- 
function(.tree)
{
  !is.null(label(.tree))
}


#' @title 
#' Root(s) of a tree
#' 
#' @description 
#' Find the root of a tree 
#' (or the multiple roots for a non-rooted tree)
#' 
#' @param .tree
#' A tree. 
#' 
#' @return 
#' \code{root} returns a node, the root of the tree if it exists;  
#' if \code{.tree} is not rooted, an error is thrown.  
#' 
#' \code{roots} returns a list of node, the roots of the tree. 
#' 
#' @export
#' 
root <- # :: Tree -> Node
function(.tree)
{
  UseMethod("root")  
}


#' @export
#' @rdname root
#' 
root.rtree <- # :: Tree -> Node
function(.tree)
{
  # si .tree est vide, faut-il renvoyer empty_tree() ?
  
  if (!is.rooted(.tree)) {
    stop("'.tree' is not a rooted tree")
  }
  no = rtree(label(.tree))
  attributes(no) = attributes(.tree)
  #class(no) = c("node", class(.tree))
  no
}


#' @export
#' @rdname root
#' 
roots <- # :: Tree -> [Node]
function(.tree)
{
  UseMethod("roots")
}


#' @export
#' @rdname root
#' 
roots.rtree <- # :: Tree -> [Node]
function(.tree)
{
  if (is.rooted(.tree)) {
    return(list(root(.tree)))
  }
  lapply(.tree, FUN = root) # ok parce qu'on a interdit qu'un sous-arbre de '.tree' soit non-rooted
}
