#' Fits a BN using covariates from rasters
#'
#' @details Given the path to a set of rasters and a region
#' specified by a polygon or set of polygons (simple feature) it adjusts a
#' Bayesian Network.
#' @param geom region sf, polygon(s) that determine analysis area
#' @param vars_paths paths to rasters with covariates to be included in BN
#' @param crs string specifying projection (proj4string) passed to
#' sf::st_transform, defaults to lcc (Mex)
#' @param score	a character string, the label of the network score to be used in
#' hill-climbing algorithm, pased to \code{\link[bnlearn]{hc}}. Defaults to BIC
#' @export
fit_bn <- function(geom, vars_paths, crs = "lcc_mex", score = "bic") {
    vars <- prep_geom_vars(vars_paths, geom, crs)
    vars_bn <- dplyr::select_if(vars$vars_df, is.factor)
    fit_bn_str(vars_bn, score = score)
}
