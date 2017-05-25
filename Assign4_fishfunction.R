#' Fish function 
#'
#' calculate maximum amounts of fish by each location, revenues by each fish species, and total revenues. 
#'
#' @param  price.table a table that contain the names of fish species in the first column, and the price for each fish species in the second columm. 
#' @param  number.fish.location a table that has the number caught for each fish species for each location. Each location should be in a different column, and each fish species is in a different row
#' @param  plotYN plot revenes by each location and total revenues if plotYN = "yes"(default="no")
#' @author Eunhee Lee, Arun Bird
#' @return
#' maximum amounts of fish by each location, revenues by each fish species, totla revenues.  
#'
#'
fishfunction = function(price.table, number.fish.location, plotYN="no") {
  maxcount = apply(number.fish.location,2,max,na.rm = TRUE)
  maxfishtable = matrix(ncol=ncol(number.fish.location))
  for (k in 1:ncol(number.fish.location)){
    rownum = as.numeric(which.max(number.fish.location[,k]))
    maxfishtable[,k]=rownames(number.fish.location)[rownum]
    colnames(maxfishtable)=colnames(number.fish.location)
  }
  fishrevtable = matrix(nrow=nrow(number.fish.location), ncol=ncol(number.fish.location))
  for (i in 1:nrow(number.fish.location)){
    for (j in 1:ncol(number.fish.location)){
      rownumber <- as.numeric(match(rownames(number.fish.location)[i],price.table[,1]))
      fishrevtable[i,j] = as.numeric(price.table[rownumber,2])*number.fish.location[i,j]}
  }
  colnames(fishrevtable)=colnames(number.fish.location)
  fishrev=apply(fishrevtable,2,FUN="sum")
  totalrev = sum(fishrev)
  fishrevtotal = fishrev 
  fishrevtotal$total = totalrev
  fishrevtotal = as.matrix(t(fishrevtotal))
  if(plotYN=="yes") {fishplot = barplot(fishrevtotal, main="Revenues by each location", col=c("green"))}
  return(list(Maxcount = maxcount, Maxfish = maxfishtable, Revenues = fishrev, Totalrevenue=totalrev))
}


