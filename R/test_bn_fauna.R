#' Tests a BN for each species on a shapefile using covariates from rasters
#'
#' @details Given a set of points observations of fauna species and a a region
#' specified by a polygon or set of polygons (simple feature) and a fiited
#' Bayesian Network per species it evaluates the likelihood given the covariates
#' given in rasters, for the fauna and the no fauna data.
#' @param fauna_geom fauna sf, point geometry indicating where species where
#' observed
#' @param species_var unquoted variable indicating column on fauna data that
#' indicates species
#' @param geom region sf, polygon(s) that determine test area
#' @param covs_paths paths to rasters with covariates to be included in BN
#' @export
test_bn_fauna <- function(bn_fitted_list, fauna_geom, species_var, geom,
    covs_paths, crs = "lcc_mex") {
    if (crs == "lcc_mex") {
        crs <- "+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +datum=WGS84 +units=m +no_defs"
    }
    fauna_covs <- prep_geom_data(fauna_geom, {{species_var}}, geom,
        covs_paths, crs = crs)
    fauna_df_list <- fauna_covs[[1]]

    fauna_df <- dplyr::bind_rows(fauna_df_list, .id = "species") %>%
        tibble::add_column(presence = TRUE)
    covs_df <- fauna_covs[[2]] %>%
        dplyr::select(-x, -y) %>%
        dplyr::distinct()
    esp_logliks <- purrr::map_dfc(bn_fitted_list, ~logLik(.$bn_fit,
        covs_df, by.sample = TRUE))
    all <- esp_logliks %>%
        dplyr::bind_cols(covs_df) %>%
        tidyr::pivot_longer(cols = dplyr::one_of(colnames(esp_logliks)),
            names_to = "species", values_to = "log_lik")
    all %>%
        dplyr::left_join(fauna_df) %>%
        dplyr::mutate(presence = ifelse(is.na(presence), 0, 1)) %>%
        dplyr::group_by(species, presence) %>%
        dplyr::summarise(
            n = dplyr::n(),
            min = min(log_lik),
            max = max(log_lik),
            median = median(log_lik)
        ) %>%
        dplyr::arrange(species, presence)
}
