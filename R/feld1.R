#' Cat Allergen Concentrations
#'
#' @description Simulated data for cat allergen concentrations (Fel d 1) in 471
#'  homes, measured in both the living room and the bed mattress.
#'
#' @format A \code{data.frame} with 471 observations on the following 5 variables:
#' \describe{
#' \item{mattress}{Feld d 1 concentration in the bed mattress (\eqn{\mu}{micro}g/g).}
#' \item{room}{Feld d 1 concentration in the living room (\eqn{\mu}{micro}g/g).}
#' \item{logmattress}{logarithm of \code{mattress}.}
#' \item{logroom}{logarithm of \code{room}.}
#' \item{cat}{a factor with levels \code{no} and \code{yes}, indicating cat ownership.}
#' }
#'
#' @details Data were simulated to emulate true data pattern observed in a real
#'  study (see References).
#'
#' @source See References.
#'
#' @references Basagana X \emph{et al}. Domestic aeroallergen levels in Barcelona
#'  and Menorca (Spain). \emph{Pediatric Allergy and Immunology}. 2002;13(6):412-417.
#'
#' @examples
#' data(feld1)
#' par(las = 1, mfrow = c(1, 2))
#' with(feld1, plot(mattress, room, col = as.numeric(cat)))
#' with(feld1, plot(logmattress, logroom, col = as.numeric(cat)))
#' @keywords datasets
"feld1"
