#' h2l() Function
#'
#' This function converts header to lower case
#' @param x :
#' @keywords h2l
#' @export
#' @examples
#' h2l(x)

h2l <- function(x) {
  names(x) <- tolower(names(x))
  return(x)
}
