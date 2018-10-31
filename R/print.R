#' @title 
#' Print trees
#' 
#' @description 
#' This function prints the labels of the nodes of a tree, 
#' and displays the connections between these nodes. 
#' 
#' @param x
#' A tree. 
#' 
#' @param at
#' character. If not \code{NULL}, the name of an attribute of the nodes of \code{x} 
#' to be printed next to the node labels.  
#' 
#' @param level
#' integer. This argument is used internally by the function, should not be used directly. 
#' 
#' @param ...
#' Additional parameters (not used). 
#' 
#' @return 
#' \code{x} is return invisibly. 
#' 
#' @seealso For examples using the \code{at} argument, 
#' see \code{\link[oak]{tree_apply}}. 
#' 
#' @export
#' 
print.rtree <-
function(x, 
         at = NULL, 
         level = 1L, 
         ...)
{
  xattr = NULL
  if (!is.null(at)) {
    xlabel = label(x)
    xattr = attr(x, at)
  } else {
    xlabel = label(x)
  }
  space = paste(rep(" ", 4L*(level-1L)), collapse = "")
  if (is.null(xattr)) {
    cat(paste0(space, "|-"), xlabel, "\n")
  } else {
    cat(paste0(space, "|-"), xlabel, paste0("(", xattr, ")"), "\n")
  }
  if (length(x) == 0L) {
    return(invisible(x))
  }
  for (i in seq_along(x)) {
    print(x[[i]], at = at, level = level + 1L, ...)
  }
  invisible(x)
}
