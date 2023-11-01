#' Applies a meta.data template onto an indicated variable.
#'

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
#' @export
apply.meta <- function( variable =NA , template = NA){
  original <- variable
  template.2 <- as.list(template)
  modified.variable <- create.meta( original,
                                    long.label  = template.2$long.label[[1]] ,
                                    full.text  = template.2$full.text[[1]] ,
                                    value.labels  = template.2$value.labels[[1]] ,
                                    missing  = template.2$missing[[1]] ,
                                    associated.ids  = template.2$associated.ids[[1]] ,
                                    comments  = template.2$comments[[1]])
  return(modified.variable)}

