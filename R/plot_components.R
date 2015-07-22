#' Plot the components of the dampener
#'
#' @param s2_index - The index used for the calculation of the dampener
#' @return ggplot of the components of the dampener
#' @details The dampener effect consists of 4 components:
#' 1. The last value of the index, 2. the average value of the index,
#' 3. a constant to subtract from the relative distance between the last and the average value of the index.
#' @examples
#' s2_index = load_data()
#' plot_components(s2_index)

plot_components <- function (s2_index) {
  library(ggplot2)
  res = calculate_dampener(s2_index)
  df = data.frame(key = c("1 - last_val","2 - av_val", "3 - const", "4 - result"),
                  val = c(as.numeric(s2_index[length(s2_index)])/(2*mean(s2_index)), .5,-0.54, res)
  )

  ggplot(df, aes(x=key, y = val, fill = key)) + geom_bar(stat = "identity")
}