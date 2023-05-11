#' Applies a meta.data template onto an indicated variable.
#'
#' @export
#'
#' @param variable is the variable that we want to apply the template to
#' @param template is the template that we want to apply to the variable.
#' @returns variable with the meta.data attached.
#' @examples
#'
#'
#'iris$Sepal.Width <- create.meta(iris$Sepal.Width,
#'                                 full.text = 'Variable indicating the Width of the Sepal',
#'                                 value.labels = 'cm',
#'                                 missing = 'NA' ,
#'                                 comments = 'this is an example of more detailed meta.data 2')
#'
#' write.meta <- function(variable) {
#' part.1 <-  as.data.frame(t(cbind(attributes(variable))))
#' return(part.1)}
#'
#'
#'meta.sepal.width <- write.meta(iris$Sepal.Width)
#'
#'
#'
#'sepal.1 <- apply.meta(iris$Sepal.Width, meta.sepal.width )
#'
#'
#'
#'
#'
#'

apply.meta <- function( variable =NA , template = NA){
  original <- variable
  template.2 <- as.list(template)
  attributes(original) <- template.2
  return(original)}
