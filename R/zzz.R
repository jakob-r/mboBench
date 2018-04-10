#' @import BBmisc
#' @import checkmate
#' @import ParamHelpers
#' @import smoof
#' @import utils
#' @import data.table
#' @import R6
#' @import parallelMap
#' @import stringi
#' @import ggplot2
#' @importFrom stats quantile
#' @importFrom stats runif
#' @importFrom purrr map
#' @importFrom digest sha1
#' @importFrom methods getFunction

.onAttach = function(libname, pkgname) {
  parallelRegisterLevels(package = "mboBench", levels = c("evalX"))
}