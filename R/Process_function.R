#' This function filters non NA observations and detrends the clean
#'  series using HP filter
#'
#'  @import mFilter
#'

detrend.HP = function(data, HP_gamma = NULL){

  non_na_ind = !is.na(data)

  HP_cycle = hpfilter(data[non_na_ind],freq = 400,type = "lambda")[["cycle"]]

  res = data

  res[non_na_ind] = HP_cycle

  return(res)

}
