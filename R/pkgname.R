#' @details
#' Some non confidential functions developped for a synthetic data project
#' 
#' @keywords internal
"_PACKAGE"
#> [1] "_PACKAGE"

#' BigSyn: Some non confidential R functions developped for the MLDSC synthetic data project
#'
#' The BigSyn package allows to synthesize big hierarchical databases by opposition to just a single small "rectangular" table.
#'  The general idea is to
#'   - provide tools to transpose the data and back transpose the synthetic version of the transposed data.
#'   - provide a synthetisation procedure that runs the modeling and the sampling separately
#'   - provide tools to operate a reasonable pre-selection of predictors.
#'   - provide tools to visualize the synthetisation.
#' @section BigSyn functions:
#' The main BigSyn functions are
#' SDPSYN2
#' Generaltransposefunction
#' GeneralReversetransposefunction
#' 
#' @section Shiny application:
#' runCompare() launches a shiny application to 
#'
#' @section General approach:
#' A step by step example is provided in the Synthesize_database demo file
#' 
#' @examples
#' demo(Synthesize_database)
#' @docType package
#' @name BigSyn
NULL