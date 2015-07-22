#' Calculate the dampener
#'
#' @param to The final date for which the index is calculated
#' @return xts value of length 1 with the size of the dampener effect
#' @examples calculate_dampener()

calculate_dampener = function(s2_index){
  res = 0.5*((s2_index[length(s2_index)]-mean(s2_index))/mean(s2_index)-0.08)
  res = max(min(res,0.1),-0.1)
  res
}