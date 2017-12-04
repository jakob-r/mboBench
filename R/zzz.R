#' @import BBmisc
#' @import checkmate
#' @import ParamHelpers
#' @import smoof
#' @import utils
#' @import data.table
#' @import R6
#' @import parallelMap

.onAttach = function(libname, pkgname) {
  parallelRegisterLevels(package = "mboBench", levels = c("evalX", "evalDesign"))
}