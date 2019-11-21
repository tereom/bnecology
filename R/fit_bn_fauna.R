#' Fits a BN for each species on a shapefile using covariates from rasters
#'
#' @details Given a set of points observations of fauna species and a region
#' specified by a polygon or set of polygons (simple feature) it adjusts a
#' Bayesian Network per species and covariates given in rasters.
#' @param fauna_geom fauna sf, point geometry indicating where species where
#' observed
#' @param species_var unquoted variable indicating column on fauna data that
#' indicates species
#' @param geom region sf, polygon(s) that determine analysis area
#' @param covs_paths paths to rasters with covariates to be included in BN
#' @export
fit_bn_fauna <- function(fauna_geom, species_var, geom, covs_paths,
    crs = "lcc_mex") {
    fauna_df_list <- prep_geom_data(fauna_geom, {{species_var}}, geom,
        covs_paths, crs = crs)[[1]]
    fauna_bn_list <- purrr::map(fauna_df_list, fit_bn_str)
    fauna_bn_list
}

prep_geom_data <- function(fauna_geom, species_var, geom, covs_paths, crs) {
    #' called by fit_bn_fauna, prepares data to fit BNs or to evaluate
    #' likelihood
    #  covs_ind: indices of rasters with covariates to consider
    #' returns a list with an entry per species, each entry contains a data
    #' frame of categorized covarites suitable for fitting/evaluating a BN
    # covariates
    covs <- prep_geom_vars(covs_paths, geom, crs)
    covs_df <- covs$vars_df
    # fauna
    fauna_df <- prep_geom_fauna(fauna_geom, geom, covs$raster_id)
    # fauna & covariates
    fauna_covs <- fauna_df %>%
        dplyr::left_join(covs_df, by = "id") %>%
        dplyr::mutate(id = factor(id))
    fauna_df_list_split <- fauna_covs %>%
        dplyr::group_split({{species_var}})
    species <- map_chr(fauna_df_list_split, ~dplyr::pull(., {{species_var}})[1])
    fauna_df_list <- rlang:::set_names(fauna_df_list_split, species) %>%
        purrr::map(prep_data_bn)
    list(fauna_df_list = fauna_df_list,
        covs_df = covs_df %>% dplyr::select(-id) %>% dplyr::distinct())
}
prep_geom_fauna <- function(fauna_geom, geom, raster_id) {
    # called by prep_geom_data intersects fauna's sf with geom and extracts
    # ids from raster_id
    fauna_geom <- fauna_geom %>%
        sf::st_intersection(sf::st_geometry(geom)) %>%
        dplyr::mutate(id = raster::extract(raster_id, .)) %>%
        sf::st_drop_geometry() %>%
        dplyr::distinct() %>%
        tidyr::drop_na()
    fauna_geom
}
prep_data_bn <- function(data_sp) {
    # called by prep_geom_data preparesdata per species (deletes dulpicates and
    # NAs)
    data_sp %>%
        dplyr::select_if(is.factor) %>%
        dplyr::distinct() %>%
        dplyr::select(-id) %>%
        tidyr::drop_na()
}
