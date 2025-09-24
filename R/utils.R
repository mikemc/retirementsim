#' Convert years to periods
#' 
#' @param years Number of years
#' @param periods_per_year Number of periods per year
#' @return Number of periods
#' @export
years_to_periods <- function(years, periods_per_year) {
  years * periods_per_year
}

#' Convert periods to years
#' 
#' @param periods Number of periods
#' @param periods_per_year Number of periods per year
#' @return Number of years
#' @export
periods_to_years <- function(periods, periods_per_year) {
  periods / periods_per_year
}

#' Annualize a periodic return
#' 
#' @param periodic_return Return for a single period
#' @param periods_per_year Number of periods per year
#' @return Annualized return
#' @export
annualize_return <- function(periodic_return, periods_per_year) {
  (1 + periodic_return)^periods_per_year - 1
}

#' Convert annual return to periodic return
#' 
#' @param annual_return Annual return
#' @param periods_per_year Number of periods per year
#' @return Periodic return
#' @export
periodize_return <- function(annual_return, periods_per_year) {
  (1 + annual_return)^(1 / periods_per_year) - 1
}