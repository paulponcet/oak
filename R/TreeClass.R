#' @title 
#' Tree Class
#' @docType class
#' 
#' @description 
#' Tree class
#' 
#' @field data 
#' object to be converted to an \code{\link[oak]{rtree}}. 
#'
#' @section Methods:
#' \describe{
#'   \item{get_tree()}{Get the tree (as an \code{\link[oak]{rtree}} object)}
#'   \item{ancestors_of(.node, include_node = FALSE)}{Get the \code{\link[oak]{ancestors}} of the node}
#'   \item{children_of(.node, degree = 1L)}{Get the \code{\link[oak]{children}} of the node}
#'   \item{descendants_of(.node, include_node = FALSE)}{Get the \code{\link[oak]{descendants}} of the node}
#'   \item{parent_of(.node, degree = 1L)}{Get the \code{\link[oak]{parent}} of the node}
#'   \item{siblings_of(.node, include_node = FALSE)}{Get the \code{\link[oak]{siblings}} of the node}
#' }
#' 
#' @seealso \code{\link[oak]{Nodes}} and \code{\link[oak]{Node}} 
#' in this package. 
#' 
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
  
  ancestors_of = function(.node, include_node = FALSE)
  {
    as = ancestors(.node, self$.tree, include_node = include_node)
    Nodes$new(labels = sapply(as, FUN = label), tree = self$.tree)
  }, 
  
  children_of = function(.node, degree = 1L)
  {
    chs = children(.node, self$.tree, degree = degree)
    Nodes$new(labels = sapply(chs, FUN = label), tree = self$.tree)
  }, 
  
  descendants_of = function(.node, include_node = FALSE)
  {
    ds = descendants(.node, self$.tree, include_node = include_node)
    Nodes$new(labels = sapply(ds, FUN = label), tree = self$.tree)
  }, 
  
  parent_of = function(.node, degree = 1L)
  {
    p = parent(.node, self$.tree, degree = degree)
    Node$new(label = label(p), tree = self$.tree)
  }, 
  
  siblings_of = function(.node, include_node  = FALSE)
  {
    s = siblings(.node, self$.tree, include_node = include_node )
    Nodes$new(labels = sapply(s, FUN = label), tree = self$.tree)
  }
  
))


#' @title 
#' Nodes Class
#' @docType class
#' 
#' @description 
#' Nodes class
#' 
#' @field labels
#' character. A vector of labels for each node. The labels must be part of 
#' the \code{tree}'s labels. 
#' @field tree
#' rtree. The tree where the nodes belong to. 
#' @field ... 
#' Additional parameters (currently not used). 
#'
#' @section Methods:
#' \describe{
#'   \item{get_labels()}{Get labels of the nodes}
#'   \item{get_tree()}{Get the tree attached to the nodes}
#'   \item{set_tree(tree)}{Set the tree}
#'   \item{keep(i)}{}
#' }
#' 
#' @seealso \code{\link[oak]{Tree}} and \code{\link[oak]{Node}} 
#' in this package. 
#' 
#' @importFrom R6 is.R6 R6Class
#' @export
#' 
Nodes <- R6::R6Class("Nodes", list(
  
  .labels = NULL,
  .tree = NULL,
  
  initialize = function(labels, tree, ...)
  {
    stopifnot(is.null(labels) || (is.character(labels) && length(labels) >= 1L))
    stopifnot(all(labels %in% labels(tree))) # TODO: si 'tree' n'est pas un rtree...
    if (is.rtree(tree)) tree = Tree$new(tree)
    stopifnot(R6::is.R6(tree) && inherits(tree, "Tree"))
    self$.labels = labels
    self$.tree = tree
  }, 
  
  get_labels = function()
  {
    self$.labels
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
    # do something with values?
    invisible(self)
  }
  
))


#' @title 
#' Node Class
#' @docType class
#' 
#' @description 
#' Node class; inherits from the class \code{\link[oak]{Nodes}}. 
#' 
#' @field label
#' character. The label of the node; must be part of 
#' the \code{tree}'s labels. 
#' @field tree
#' rtree. The tree where the node belongs to. 
#' @field ... 
#' Additional parameters (currently not used). 
#'
#' @section Methods:
#' \describe{
#'   \item{get_label()}{Get the label of the node}
#'   \item{ancestors(include_node = FALSE)}{Get the \code{\link[oak]{ancestors}} of the node}
#'   \item{children(degree = 1L)}{Get the \code{\link[oak]{children}} of the node}
#'   \item{descendants(include_node = FALSE)}{Get the \code{\link[oak]{descendants}} of the node}
#'   \item{parent(degree = 1L)}{Get the \code{\link[oak]{parent}} of the node}
#'   \item{siblings(include_node = FALSE)}{Get the \code{\link[oak]{siblings}} of the node}
#' }
#' 
#' @seealso \code{\link[oak]{Tree}} and \code{\link[oak]{Nodes}} 
#' in this package. 
#' 
#' @importFrom R6 R6Class
#' @export
#' @rdname Tree
#' 
Node <- R6::R6Class("Node", list(
  
  .labels = NULL,
  .tree = NULL,
  
  initialize = function(label, tree, ...)
  {
    # label can be NULL or "" when using 'parent'...
    stopifnot(is.null(label) || (is.character(label) && length(label) == 1L))
    super$initialize(labels = label, tree = tree, ...)
  }, 
  
  get_label = function()
  {
    self$get_labels()
  }, 
  
  #is_leafnode = function()
  #{
    # TODO
  #}, 
  
  ancestors = function(include_node = FALSE)
  {
    self$.tree$ancestors_of(self$.labels, include_node = include_node)
  }, 
  
  children = function(degree = 1L)
  {
    self$.tree$children_of(self$.labels, degree = degree)
  }, 
  
  descendants = function(include_node = FALSE)
  {
    self$.tree$descendants_of(self$.labels, include_node = include_node)
  }, 
  
  parent = function(degree = 1L)
  {
    self$.tree$parent_of(self$.labels, degree = degree)
  }, 
  
  siblings = function(include_node = FALSE)
  {
    self$.tree$siblings_of(self$.labels, include_node = include_node)
  }), 
  
  inherit = Nodes
)
