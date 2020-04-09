#' Covid-19 Datasets
#'
#' @description
#' Covid-19 datasets various resources.
#'
#' \code{c19}: John-Hopkins University
#'
#' @details
#' Some countries which has provinces were seperated from the original data source.
#' These countries are Autralia, Canada, China, Denmark, France, United Kingdom
#' and Netherlands. You can see long format of these data sources by adding
#' 3-letters iso code at the end of 'c19.' string i.e., 'c19.gbr'.
#'
#' \code{[dataset_name]w}: wide format
#'
#' \code{[dataset_name]l}: long format
#'
#' @usage c19w # wide format
#' @source \url{https://raw.githubusercontent.com/CSSEGISandData/COVID-19/}
"c19"

#' @usage c19l # long format
#' @rdname c19
"c19l"
