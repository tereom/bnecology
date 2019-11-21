#' Creates a data.frame with covariates from rasters.
#'
#' @details Reads tif files of discretized variables and id variable,
#' cuts rasters with polygons in a polygon simple feature and returns a
#' data.frame with the categorical variables, coordinates and id variable.
#' @param vars_paths paths to rasters with covariates to consider, must include
#' raster with id's (name of layer should contain "id")
#' @param geom region sf, polygon(s) that determine analysis area
#' @param crs string specifying projection (proj4string) passed to
#' sf::st_transform, defaults to lcc (Mex)
#' @export
prep_geom_vars <- function(vars_paths, geom, crs = "lcc_mex") {
    if (crs == "lcc_mex") {
        crs <- "+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +datum=WGS84 +units=m +no_defs"
    }
    vars_names <- tools::file_path_sans_ext(basename(as.character(vars_paths)))
    vars_paths <- purrr::set_names(vars_paths, vars_names)
    vars_list <- purrr::map(vars_paths, raster::raster) %>%
        purrr::map(raster::crop, y = geom) %>%
        purrr::map(raster::mask, mask = geom)
    vars_brick <- vars_list %>%
        raster::stack()
    vars_df <- raster::as.data.frame(vars_brick, xy = TRUE, na.rm = FALSE) %>%
        dplyr::mutate_if(is.factor, ~forcats::fct_drop(., only = "")) %>%
        tidyr::drop_na()
    colnames(vars_df) <- stringr::str_remove(colnames(vars_df), "_category")
    # discretize zone life
    if ("zvh_p_hgw" %in% colnames(vars_df)) {
        vars_df$zvh_p_hgw <- factor(vars_df$zvh_p_hgw, levels = 1:11)
    } else if ("zvh_31" %in% colnames(vars_df)) {
        vars_df$zvh_31 <- factor(vars_df$zvh_31, levels = 1:31)
    } else if ("zz_delta_vp" %in% colnames(vars_df)) {
        vars_df$zz_delta_vp <- factor(vars_df$zz_delta_vp, levels = 1:9)
    }
    list(vars_df = dplyr::rename(vars_df, id = dplyr::contains("id")),
        raster_id = vars_list[[stringr::str_subset(names(vars_list), "id")]])
}
