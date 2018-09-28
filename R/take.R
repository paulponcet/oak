
find_path_to_node <- # :: Node -> Tree -> String
function(.node, 
         .tree)
{
  UseMethod("find_path_to_node", .tree)
}


find_path_to_node.rtree <- # :: Node -> Tree -> String
function(.node, 
         .tree)
{
  if (is.node(.node)) {
    .node = label(.node)
  }
  #path = integer(0L)
  if (is.root(.node, .tree) && is.rooted(.tree)) {
    return(integer(0L))
    
  } else if (is.root(.node, .tree) && !is.rooted(.tree)) {
    l = sapply(roots(.tree), label)
    return(which(l == .node))
    
  } else {
    p = parent(.node, .tree)
    ## If '.node' does not exist in '.tree': 
    if (is.empty(p)) {
      return(NULL)
    }
    path = find_path_to_node(p, .tree)
    l = sapply(children(p, .tree), label)
    w = which(l == .node)
    c(path, w)
  }
}


#' @title
#' Take a branch of a tree
#' 
#' @description
#' The function \code{take_branch} looks for the subtree of
#' \code{.tree} whose root is the node identified by \code{.node}.
#' 
#' @param .tree
#' A tree.
#' 
#' @param .node
#' A node of \code{.tree}. 
#' 
#' @return
#' A tree.
#' 
#' @export
# #' @rdname find_path_to_node
#' 
take_branch <- 
function(.tree, 
         .node)
{
  UseMethod("take_branch")
} 


#' @export
#' @rdname take_branch
#' 
take_branch.rtree <- 
function(.tree, 
         .node)
{
  path = find_path_to_node(.node, .tree)
  if (is.null(path)) {
    empty_tree()
  } else if (length(path) == 0L) {
    .tree
  } else {
    .tree[[path]]
  }
}


#' @export
#' @rdname take_branch
#' 
take_node <-
function(.node, 
         .tree)
{
  root(take_branch(.tree, .node))
}


# TODO

# #' @export
# # #' @rdname find_path_to_node
# #' 
# cut_branch2 <- 
# function(.tree, 
#          .node)
# {
#   path = find_path_to_node(.node, .tree)
#   .tree[[path]] = NULL
#   .tree
# }


# #' @export
# #' 
# "[.rtree" <- 
# function(x,  # tree
#          i,  # node or node label
#          ...)
# {
#   if (is.node(i)) {
#     i = label(i)
#   }
#   take_branch(x, i)
# }


# #' @export
# #' 
# "[<-.rtree" <- 
# function(x, i, value)
# {
#   path = find_path_to_node(i, x)
#   x[[path]] = i
#   x
# }


# find_branch <- # :: Tree -> Node -> Tree
# function(.tree, 
#          .node = NULL)
# {
#   UseMethod("find_branch")
# }


# #' @export
# #' @rdname find_branch
# #' 
# find_branch.rtree <- # :: Tree -> Node -> Tree
# function(.tree, 
#          .node = NULL)
# {
#   if (missing(.node) || is.empty(.tree)) {
#     return(.tree)
#   }
#   if (is.node(.node)) {
#     .node = label(.node)
#   }
#   if (is.null(.node)) {
#     return(.tree)
#   }
#   if (is.rooted(.tree) && is.root(.node, .tree)) {
#     return(.tree)
#   }
#   r = lapply(.tree, FUN = find_branch.rtree, .node = .node)
#   w = which(!sapply(r, is.empty))
#   if (length(w) > 0L) {
#     r[[w]]
#   } else {
#     empty_tree()
#   }
# }
