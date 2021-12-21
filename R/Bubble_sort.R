#' @title bubble sort
#' @description bubble sort
#' @param vector a vector
#' @return A sorted vector
#' @export
Bubble_sort = function(vector) {
  n = length(vector)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      if(vector[i]>=vector[j]){
        temp = vector[i]
        vector[i] = vector[j]
        vector[j] = temp
      }
    }
  }
  return(vector)
}