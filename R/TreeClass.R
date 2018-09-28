
#' @importFrom R6 R6Class
#' @export
#' 
Tree <- R6::R6Class("Tree", list(
  
  .tree = NULL, 
  
  initialize = function(data)
  {
    self$.tree = as.rtree(data)
  }, 
  
  # To be added: is_node, get_node, ...
  
  get_tree = function()
  {
    self$.tree
  }, 
  
  ancestors_of = function(.node)
  {
    as = ancestors(.node, self$.tree)
    Nodes$new(labels = sapply(as, FUN = label), tree = self$.tree)
  }, 
  
  children_of = function(.node, degree = 1L)
  {
    chs = children(.node, self$.tree, degree = degree)
    Nodes$new(labels = sapply(chs, FUN = label), tree = self$.tree)
  }, 
  
  descendants_of = function(.node)
  {
    ds = descendants(.node, self$.tree)
    Nodes$new(labels = sapply(ds, FUN = label), tree = self$.tree)
  }, 
  
  parent_of = function(.node, degree = 1L)
  {
    p = parent(.node, self$.tree, degree = degree)
    Node$new(label = label(p), tree = self$.tree)
  }, 
  
  siblings_of = function(.node)
  {
    s = siblings(.node, self$.tree)
    Nodes$new(labels = sapply(s, FUN = label), tree = self$.tree)
  }
  
))


#' @importFrom R6 is.R6 R6Class
#' @export
#' 
Nodes <- R6::R6Class("Nodes", list(
  
  .labels = NULL,
  .values = NULL,
  .tree = NULL,
  
  initialize = function(labels, values = NULL, tree, ...)
  {
    stopifnot(is.null(labels) || (is.character(labels) && length(labels) >= 1L))
    stopifnot(all(labels %in% labels(tree)))
    if (is.rtree(tree)) tree = Tree$new(tree)
    stopifnot(R6::is.R6(tree) && inherits(tree, "Tree"))
    self$.labels = labels
    self$.values = values
    self$.tree = tree
  }, 
  
  get_labels = function()
  {
    self$.labels
  }, 
  
  set_labels = function(...)
  {
    stop("cannot change 'labels' attribute")
  }, 
  
  get_values = function()
  {
    self$.values
  }, 
  
  set_values = function(values)
  {
    self$.values = values
    invisible(self)
  }, 
  
  get_tree = function()
  {
    self$.tree
  }, 
  
  set_tree = function(tree)
  {
    self$.tree = tree
    invisible(self)
  }, 
  
  keep = function(i)
  {
    if (is.character(i)) {
      i = which(self$.labels %in% i)
    }
    stopifnot(length(i) > 0L)
    self$.labels = self$.labels[i]
    invisible(self)
  }
  
))


#' @importFrom R6 R6Class
#' @export
#' 
Node <- R6::R6Class("Node", list(
  
  .labels = NULL,
  .values = NULL,
  .tree = NULL,
  
  initialize = function(label, value = NULL, tree, ...)
  {
    # label can be NULL or "" when using 'parent'...
    stopifnot(is.null(label) || (is.character(label) && length(label) == 1L))
    super$initialize(labels = label, values = value, tree = tree, ...)
  }, 
  
  get_label = function()
  {
    self$get_labels()
  }, 
  
  set_label = function(...)
  {
    stop("cannot change 'label' attribute")
  }, 
  
  get_value = function()
  {
    self$get_values()
  }, 
  
  set_value = function(...)
  {
    self$set_values(...)
  }, 
  
  is_leafnode = function()
  {
    # TODO
  }, 
  
  ancestors = function()
  {
    self$.tree$ancestors_of(self$.labels)
  }, 
  
  children = function(degree = 1L)
  {
    self$.tree$children_of(self$.labels, degree = degree)
  }, 
  
  descendants = function()
  {
    self$.tree$descendants_of(self$.labels)
  }, 
  
  parent = function(degree = 1L)
  {
    self$.tree$parent_of(self$.labels, degree = degree)
  }, 
  
  siblings = function()
  {
    self$.tree$siblings_of(self$.labels)
  }), 
  
  inherit = Nodes
)
