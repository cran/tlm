#' Intima Media Thickness of the Carotid Artery
#'
#' @description Simulated data for intima media thickness of the carotid artery
#'  and age in 2784 adults.
#'
#' @format A \code{data.frame} with 2784 observations on the following 3 variables:
#' \describe{
#' \item{age}{age of the individual (years).}
#' \item{imt}{intima media thickness of the carotid artery (mm).}
#' \item{logimt}{logarithm of \code{imt}.}
#' }
#'
#' @details Data were simulated to emulate true data pattern observed in a real
#'  study (see References).
#'
#' @source See References.
#'
#' @references Rivera M \emph{et al}. Association between long-term exposure to
#'  traffic-related air pollution and subclinical atherosclerosis: the REGICOR
#'  Study. \emph{Environmental Health Perspectives}. 2013;121(2):223-230.
#'
#' @examples
#' data(imt)
#' par(las = 1, mfrow = c(1, 2))
#' with(imt, plot(age, imt))
#' with(imt, plot(age, logimt))
#' @keywords datasets
"imt"
