#' Edits the meta.data of an indicated variable.
#' @export
#'
#' @param variable is a set of variables that you want to get the meta.data of
#' @param long.label is for indicating the long name of a variable
#' @param full.text is the full question of the text
#' @param value.labels indicates the values e.g., 'cm'
#' @param missing is missing indicator
#' @param associated.ids is a list of other ids associated with the variable.
#' @param comments is for anything else you want to include
#' @param replace T or F whether the original meta.data should be replaced.
#' @returns variable with the meta.data attached.
#'
#'
#' @examples
#'
#' create.meta <- function( variable =NA , long.label =NA , full.text = NA ,
#'  value.labels =NA , missing =NA , associated.ids =NA , comments =NA ){
#'  original <- variable #make a copy
#'  part.1 <-  as.list(strsplit(as.character(substitute(variable)), '$'))
#'  attributes(original)$variable.label <- part.1[[length(part.1)]]
#'  #the rest are just normal comments etc.
#'  attributes(original)$long.label	 <- c(	long.label	)
#'  attributes(original)$full.text	 <- c(	full.text	)
#'  attributes(original)$value.labels	 <- c(	value.labels	)
#'  attributes(original)$missing	 <- c(	missing	)
#'  attributes(original)$associated.ids	 <- c(associated.ids)
#'  attributes(original)$comments <- c(	comments)
#'   return(original)}
#'
#' iris$Sepal.Length <- create.meta(iris$Sepal.Length,
#' full.text = 'Variable indicating the length of the Sepal',
#' long.label = 'original',
#' value.labels = 'cm',
#' missing = 'NA' ,
#' comments = 'this is an example of more detailed meta.data')
#'
#' change.meta(iris$Sepal.Length, long.label = 'test.1',
#'         full.text = 'this is another test of something',
#'         associated.ids = 'this is new one',
#'         replace = TRUE)



change.meta <- function(variable =NA ,
                       long.label = NA ,
                       full.text =NA ,
                       value.labels =NA ,
                       missing =NA ,
                       associated.ids =NA ,
                       comments =NA,
                       replace = F){

  if (!is.na(long.label)) {if (replace == T ) { attr(variable, which = 'long.label') <- long.label} else
  { attr(variable, which = 'long.label') <-  paste(long.label, attr(variable, which = 'long.label'), sep = ' (new:old) ')}}

  if (!is.na(full.text)) {if (replace == T ) { attr(variable, which = 'full.text') <- full.text} else
  { attr(variable, which = 'full.text') <-  paste(full.text, attr(variable, which = 'full.text'), sep = ' (new:old) ')}}

  if (!is.na(value.labels)) {if (replace == T ) { attr(variable, which = 'value.labels') <- value.labels} else
  { attr(variable, which = 'value.labels') <-  paste(value.labels, attr(variable, which = 'value.labels'), sep = ' (new:old) ')}}

  if (!is.na(missing)) {if (replace == T ) { attr(variable, which = 'missing') <- missing} else
  { attr(variable, which = 'missing') <-  paste(missing, attr(variable, which = 'missing'), sep = ' (new:old) ')}}

  if (!is.na(associated.ids)) {if (replace == T ) { attr(variable, which = 'associated.ids') <- associated.ids} else
  { attr(variable, which = 'associated.ids') <-  paste(associated.ids, attr(variable, which = 'associated.ids'), sep = ' (new:old) ')}}

  if (!is.na(comments)) {if (replace == T ) { attr(variable, which = 'comments') <- comments} else
  { attr(variable, which = 'comments') <-  paste(comments, attr(variable, which = 'comments'), sep = ' (new:old) ')}}

  return(variable)
}


