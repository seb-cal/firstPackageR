#' Interactive plot of daily accidents
#'
#' Plot the sum of accidents by day 
#'
#'
#' @return None
#'
#' @examples
#' dailyAcc()
#'
#' @export
dailyAcc <- function() {
  debut=min(acc$date)
  fin=max(acc$date)
  
  dates=debut:fin
  #potential for C++ here
  freq=sapply(dates, function(x) sum(acc$date==x))
  
  myTS=ts(data = freq,start = debut,end = fin)
  
  dygraph(myTS, main = "Nombre d'accidents par jours") %>%
    dyRangeSelector(dateWindow = c("2007-01-01", "2008-01-01"))
}
