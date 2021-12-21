#' @title Select sort
#' @description Select sort
#' @param vector a vector
#' @return A sorted vector
#' @export
Select_Sort = function(vector){
  n = length(vector)
  for(i in 1:(n-1)){
    minIndex = i
    for(j in (i+1):n){
      if(vector[minIndex]>vector[j]){
        minIndex = j
      }
    }
    temp = vector[i]
    vector[i] = vector[minIndex]
    vector[minIndex] = temp
  }
  return(vector)
}