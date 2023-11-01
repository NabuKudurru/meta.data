

#' Writes a meta.data file for any range of variables e.g., a scale or dataset,
#' @param variable.set is a data.frame containing multiple variables, which we want to get the meta.data of.
#' @returns a data frame, containing all of the attributes from each of the variables in the data.set.

#' @examples
#'
#' iris$Sepal.Length <- create.meta(iris$Sepal.Length,
#'    full.text = 'Variable indicating the length of the Sepal',
#'    value.labels = 'cm',
#'    missing = 'NA' ,
#'    comment = 'this is an example of more detailed meta.data')
#'
#' iris$Sepal.Width <- create.meta(iris$Sepal.Width,
#'    full.text = 'Variable indicating the Width of the Sepal',
#'    value.labels = 'cm',
#'    missing = 'NA' ,
#'    comment = 'this is an example of more detailed meta.data 2')
#'
#' iris$Petal.Length <- create.meta(iris$Petal.Length,
#'    full.text = 'Variable indicating the length of the petal',
#'    value.labels = 'cm',
#'    missing = 'NA' ,
#'    comment = 'this is an example of more detailed meta.data 3 ')
#' iris$Petal.Width <- create.meta(iris$Petal.Width,
#'    full.text = 'Variable indicating the width of the petal',
#'    value.labels = 'cm',
#'    missing = 'NA' ,
#'    comment = 'this is an example of more detailed meta.data 4 ')
#'
#' iris$Species  <- create.meta(iris$Species,
#'   full.text = 'Variable indicating the length of the Sepal',
#'   value.labels = 'cm',
#'   missing = 'NA' ,
#'   comment = 'this is an example of more detailed meta.data 5 ')
#'
#'
#' write.metas(iris)
#'
#' @import dplyr
#' @export

write.metas <- function(variable.set) {
  part.2 <-  write.meta(variable.set[,1]) # creates a template to build from (in case it has nonstandard)
  for (i in 2:length(variable.set)){ # for the other variables, of the length of this dataset, run below
    write.meta <- function(variable) {
      if (is.null(attributes(variable)))
      {missing <- create.meta(variable )
      part.1 <-  as.list(strsplit(as.character(substitute(variable)), '$'))
      attributes(temp.1 )$variable.label <- part.1[[length(part.1)]]
      temp1 <- as.data.frame(rbind(attributes(missing)))
      return(temp1)}
      else { temp1 <-  as.data.frame(rbind(attributes(variable)))}
      return(temp1)}
    part.1 <-  write.meta(variable.set[,i]) #write meta for that variable
    part.2 <- dplyr::bind_rows(part.2, part.1)} # attach it to the others
  part.3 <- apply(part.2,2,as.character)
  return(part.3)}

