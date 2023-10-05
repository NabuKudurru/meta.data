

#' Creates a new meta.data template for one particular variable.
#' @param variable a target variable.
#' @param long.label a long label for the variable.
#' @param full.text full text of the question
#' @param value.labels the value labels in the variable (e.g., cm)
#' @param missing missing indicator
#' @param associated.ids these are ids for e.g., a dataset, a scale, or etc.
#' @param comments anything else that does not have a space so far.
#'
#' @returns a variable with the attributes attached.

#'
#' @examples
#' iris$Sepal.Length <- create.meta(iris$Sepal.Length,
#'  full.text = 'Variable indicating the length of the Sepal',
#'  long.label = 'this indicates a variable measuring the length of the sepal',
#'  value.labels = 'cm' ,
#'  missing = 'NA' ,
#'  associated.ids = 'doi, link to iris data.set, links to other tools etc',
#'  comments = 'this is an example of more detailed meta.data')
#'
#'
#'
#' iris$Sepal.Width <- create.meta(iris$Sepal.Width,
#'  full.text = 'Variable indicating the Width of the Sepal',
#'  value.labels = 'cm',
#'  missing = 'NA' ,
#'  comments = 'this is an example of more detailed meta.data 2')
#'
#'
#'
#'
#'
#' @export

create.meta <- function( variable =NA , long.label =NA , full.text =NA ,
          value.labels =NA , missing =NA , associated.ids =NA , comments =NA ){#Define areas
  original <- variable #make a copy
  part.1 <-  as.list(strsplit(as.character(substitute(variable)), '$'))
  attributes(original)$variable.label <- part.1[[length(part.1)]]
  #the rest are just normal comments etc.
  attributes(original)$long.label	 <- c(	long.label	)
  attributes(original)$full.text	 <- c(	full.text	)
  attributes(original)$value.labels	 <- c(	value.labels	)
  attributes(original)$missing	 <- c(	missing	)
  attributes(original)$associated.ids	 <- c(associated.ids)
  attributes(original)$comments <- c(substitute(comments))
  return(original)}



#This can be either
