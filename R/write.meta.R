
#' This function gathers the attributes of a variable and creates a data.frame
#'
#' @param variable is a variable that you want to get the meta.data/ attributes of.
#' @returns a data.frame with the meta.data for the variable attached.
#'
#' @examples
#' create.meta <- function( variable =NA , long.label = NA , full.text = NA
#' , value.labels = NA , missing = NA , associated.ids = NA , comment = NA ){
#'  original <- variable #make a copy
#'  part.1 <-  as.list(strsplit(as.character(substitute(variable)), '$'))
#'  attributes(original)$variable.label <- part.1[[length(part.1)]]
#'  #the rest are just normal comments etc.
#'  attributes(original)$long.label	 <- c(	long.label	)
#'  attributes(original)$full.text	 <- c(	full.text	)
#'  attributes(original)$value.labels	 <- c(	value.labels	)
#'  attributes(original)$missing	 <- c(	missing	)
#'  attributes(original)$associated.ids	 <- c(associated.ids)
#'  attributes(original)$comment <- c(	comment)
#'   return(original)}
#'
#' iris$Sepal.Width <- create.meta(iris$Sepal.Width,
#'  full.text = 'Variable indicating the Width of the Sepal',
#'  value.labels = 'cm',
#'  missing = 'NA' ,
#'  comment = 'this is an example of more detailed meta.data')
#'
#' write.meta(iris$Sepal.Width)
#'
#'
#'
#' @export



 write.meta <- function(variable) {
   if (is.null(attributes(variable)))
   {missing <- create.meta(variable )
   part.1 <-  as.list(strsplit(as.character(substitute(variable)), '$'))
   attributes(temp.1 )$variable.label <- part.1[[length(part.1)]]
      temp1 <- as.data.frame(rbind(attributes(missing)))
      return(temp1)}
   else { temp1 <-  as.data.frame(rbind(attributes(variable)))}
   return(temp1)}





