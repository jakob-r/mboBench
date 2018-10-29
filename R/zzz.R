#' @import BBmisc
#' @import checkmate
#' @import ParamHelpers
#' @import smoof
#' @import utils
#' @import data.table
#' @import R6
#' @import stringi
#' @import ggplot2
#' @importFrom purrr map partial map_chr map_dbl map_int
#' @importFrom foreach foreach
#' @importFrom stats runif quantile aggregate integrate median setNames stepfun
#' @importFrom digest sha1
#' @importFrom methods getFunction

`%do%` = foreach::`%do%`