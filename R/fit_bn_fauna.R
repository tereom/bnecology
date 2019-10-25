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
    if (crs == "lcc_mex") {
        crs <- "+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +datum=WGS84 +units=m +no_defs"
    }
    fauna_df_list <- prep_geom_data(fauna_geom, {{species_var}}, geom,
        covs_paths, crs = crs)[[1]]
    fauna_bn_list <- purrr::map(fauna_df_list, bn_species_fit)
    fauna_bn_list
}

prep_geom_data <- function(fauna_geom, species_var, geom, covs_paths, crs) {
    #' called by fit_bn_fauna, prepares data to fit BNs or to evaluate
    #' likelihood
    #  covs_ind: indices of rasters with covariates to consider
    #' returns a list with an entry per species, each entry contains a data
    #' frame of categorized covarites suitable for fitting/evaluating a BN
    # covariates
    covs <- prep_geom_covs(covs_paths, geom)
    covs_df <- covs$covs_df
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

prep_geom_covs <- function(covs_paths, geom) {
    #' Creates a data.frame with covariates from rasters.
    #'
    #' Reads tif files of discretized covariates and id variable,
    #' cuts rasters with polygons in geom, returns data.frame with numerical
    #' variables (except for coordinates and id)
    #' covs_paths paths to rasters with covariates to consider
    #' geom sf to cut and mask brick of covariates
    #'
    covs_names <- tools::file_path_sans_ext(basename(covs_paths))
    covs_paths <- purrr::set_names(covs_paths, covs_names)
    covs_list <- purrr::map(covs_paths, raster::raster) %>%
        purrr::map(raster::crop, y = geom) %>%
        purrr::map(raster::mask, mask = geom)

    covs_brick <- covs_list %>%
        raster::stack()
    covs_df <- raster::as.data.frame(covs_brick, xy = TRUE, na.rm = FALSE) %>%
        dplyr::mutate_if(is.factor, ~forcats::fct_drop(., only = "")) %>%
        tidyr::drop_na()
    # discretize zone life
    if ("zvh_p_hgw" %in% colnames(covs_df)) {
        covs_df$zvh_p_hgw <- factor(covs_df$zvh_p_hgw, levels = 1:11)
    } else if ("zvh_31" %in% colnames(covs_df)) {
        covs_df$zvh_31 <- factor(covs_df$zvh_31, levels = 1:31)
    }
    list(covs_df = dplyr::rename(covs_df, id = dplyr::contains("id")),
        raster_id = covs_list[[stringr::str_subset(names(covs_list), "id")]])
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
bn_species_fit <- function(bn_data, bn_str) {
    #' called by fit_bn_fauna, Fits BN to subset of data for a given species
    #' bn_str: BN structure can be fixed
    bn_data <- as.data.frame(bn_data)
    if (missing(bn_str)) {
        bn_str <- bnlearn::hc(bn_data, score = 'k2')
    }
    bn_fit <- bnlearn::bn.fit(bn_str, bn_data, method = 'bayes', iss = 4)
    list(bn_fit = bn_fit, bn_str = bn_str)
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
