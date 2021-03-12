fraction <- function(x,y,position.bdp){
  i <- 1
  adp <- ""     # above the decimal point
  bdp <- ""     # below the decimal point
  
  r <- trunc(x/y)
  
  if (r > 0) {
    adp <- as.character(r)
    x <- (x - r*y) * 10
  } else {
    adp <- "0"
    x <- x * 10
  }
  
  for (i in 1:position.bdp) {
    r <- trunc(x/y)
    bdp <- paste(bdp, as.character(r), sep = "")
    x <- (x - r*y) * 10
  }
  
  return(paste(adp, bdp, sep = "."))
}

x <- 32412341
y <- 97456737

fraction(x,y,20)
fraction(x,y,100)
