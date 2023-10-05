

#' Applies meta.data.templates to a range of variables.
#' @import dplyr

#' @param variable.set is a data.frame containing multiple variables, which we want to apply meta.data to.
#' @param template.set is a set of templates, that we want to apply onto the variables indicated in variable.set.
#' @returns a data frame, containing the variables with the templates applied.
#'
#' @examples
#'
#'
#'
#'iris$Sepal.Length <- create.meta(iris$Sepal.Length,
#'                                 full.text = 'Variable indicating the length of the Sepal',
#'                                 value.labels = 'cm',
#'                                 missing = 'NA' ,
#'                                 comments = 'this is an example of more detailed meta.data')
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
#'metas.1 <- rbind(write.meta(iris$Sepal.Length), write.meta(iris$Sepal.Width))
#'
#'
#'
#'apply.metas(iris[,1:2], metas.1)
#'
#'
#'
#' @export



apply.metas <- function(variable.set = NA, template.set = NA) {
  edit.variable.set <- variable.set#creates an editable variable set
  edit.template.set <- template.set#creates editable template set.
  #are the template and variable set the same length?
  apply.meta <- function( variable =NA , template = NA){
    original <- variable
    template.2 <- as.list(template)
    attributes(original) <- template.2
    return(original)}
  for (i in 1:length(edit.variable.set)){ #for each variable in edit template
    edit.variable.set[,i]  <- apply.meta(edit.variable.set[,i] , edit.template.set[i,] ) }
  return(edit.variable.set)
}





