#' @title 
#' Chain constructor
#' 
#' @description 
#' The function \code{chain} creates an 'rtree' object which is a chain, 
#' i.e. a totally ordered tree. 
#' 
#' @param ...
#' characters or nodes. 
#' 
#' @return 
#' A chain. 
#' 
#' @export
#' 
chain <- 
function(...)
{
  UseMethod("chain")
}


#' @export
#' @rdname chain
#' 
chain.rtree <- 
function(...)
{
  x = list(...)
  tr = x[[1L]]
  if (!is.node(tr)) {
    stop("incorrect arguments in 'chain'")
  }
  if (length(x)==1L) {
    return(tr)
  }
  tr[[1L]] = do.call("chain", x[-1L])
  tr
}


#' @export
#' @rdname chain
#' 
chain.list <- # list of nodes
function(...)
{
  x = list(...)[[1L]]
  tr = x[[1L]]
  if (!is.node(tr)) {
    stop("incorrect arguments in 'chain'")
  }
  if (length(x)==1L) {
    return(tr)
  }
  tr[[1L]] = do.call("chain", x[-1L])
  tr
}


#' @export
#' @rdname chain
#' 
chain.numeric <- 
function(...)
{
  x = unlist(list(...))
  if (length(x)==1L) {
    chain(character(as.integer(x)))
  } else {
    chain(as.character(x))
  }
}


#' @export
#' @rdname chain
#' 
chain.character <- 
function(...)
{
  x = unlist(list(...))
  if (length(x)==1L) {
    rtree(x)
  } else {
    rtree(x[1L], subtrees = list(chain(x[-1L])))
  }
}


#' @export
#' @rdname chain
#' 
c_ <- chain
