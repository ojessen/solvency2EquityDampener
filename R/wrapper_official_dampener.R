#' Calculate the dampener based on official index
#'
#' @param to The final date for which the index is calculated
#' @return xts value of length 1 with the size of the dampener effect
#' @examples wrapper_official_dampener()



wrapper_official_dampener = function(to = Sys.Date()){
  index_para = load_index_para()
  s2_index = load_data(to=to, index_para=index_para)
  dampener = calculate_dampener(s2_index)
  dampener
}