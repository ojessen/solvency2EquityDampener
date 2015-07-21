calculate_dampener = function(to = Sys.Date()){
  to = as.Date(to)
  s2_index = load_data(to)
  res = 0.5*((s2_index[length(s2_index)]-mean(s2_index))/mean(s2_index)-0.08)
  res = max(min(res,0.1),-0.1)
  xts(res,order.by = as.Date(to))
}