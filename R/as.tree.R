#' @title 
#' Conversion to an 'rtree' object
#' 
#' @description 
#' These methods convert an object to an \code{rtree} object. 
#' 
#' @param x
#' An object to be converted. 
#' 
#' @param ...
#' Additional parameters (not used). 
#' 
#' @return 
#' An \code{rtree} object. 
#' 
#' @export
#' 
as.rtree <- 
function(x, 
         ...)
{
  UseMethod("as.rtree")
}


#' @export
#' @rdname as.rtree
#' 
as.rtree.rtree <-
function(x, 
         ...)
{
  x
}


#' @export
#' @rdname as.rtree
#' 
as.rtree.character <- 
function(x, 
         ...)
{
  rtree(x, ...)
}


#' @importFrom foreach foreach %do%
#' @export
#' @rdname as.rtree
#' 
#' @examples 
#' ## Rooted tree
#' df = data.frame(x = c("A", "A", "A", "A"), 
#'                 y = c("B", "C", "C", "C"), 
#'                 z = c("D", "E", "E", "F"), 
#'                 stringsAsFactors = FALSE)
#'                 
#' (as.rtree(df))
#' 
#' ## Unrooted tree
#' df = data.frame(x = c("A", "A", "A", "X"), 
#'                 y = c("B", "C", "C", "Y"), 
#'                 z = c("D", "E", "E", "Z"), 
#'                 stringsAsFactors = FALSE)
#'                 
#' (as.rtree(df))
#' 
as.rtree.data.frame <- 
function(x, 
         ...)
{
  stopifnot(is.character(x[[1L]]))
  col1 = unique(x[[1L]])
  if (length(col1) == 1L) {
    tr = rtree(col1)
    if (ncol(x) == 1L) return(tr)
    i = 2L
  } else {
    tr = rtree()
    i = 1L
  }
  v = ""
  sub = foreach::foreach(v = unique(x[[i]])) %do% {
    as.rtree(x[x[[i]]==v, i:ncol(x), drop = FALSE])
  }
  update(tr, sub)
}


## TO BE CHECKED...
## Currently does not work since I force labels to be unique
# as.rtree.dendogram <- 
# function(x, 
#          ...)
# {
#   at = attributes(x)
#   if (!is.null(at$leaf) && at$leaf) {
#     no = rtree(at$label)
#     at$class = c("rtree", "tree")
#     attributes(no) = at
#     return(no)
#   }
#   sub = lapply(x, FUN = as.rtree.dendogram)
#   lab = ifelse(is.null(at$label), "", at$label)
#   tr = rtree("", subtrees = sub)
#   at$class = c("rtree", "tree")
#   at$label = lab
#   attributes(tr) = at
#   tr
# }
