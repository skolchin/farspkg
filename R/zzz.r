.onLoad <- function(libname = find.package("farspkg"), pkgname = "farspkg"){
  
  # CRAN Note avoidance
  if(getRversion() >= "2.15.1") 
    utils::globalVariables(
      c("MONTH", "n", "STATE", "year")
    )
}
