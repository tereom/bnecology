#' Reads shapefiles, projects to given crs and returns point simple feature
#'
#' Recieves path(s) to shapefile(s), projects to given crs and returns a single
#' simple feature.
#' @param shp_paths vector of path to shape files (or single path)
#' @param crs string specifying projection (proj4string) passed to
#' sf::st_transform, defaults to lcc (Mex)
#' @param filter_var optional, if filtering by a variable (for example year)
#' unquoted variable corresponding to the variable name
#' @param filter_values optional, values to keep of filter_var
#' @importFrom magrittr %>%
#' @export
read_shps_points <- function(shp_paths, crs = "lcc_mex", filter_var,
    filter_values) {
    if (crs == "lcc_mex") {
        crs <- "+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +datum=WGS84 +units=m +no_defs"
    }
    if (length(shp_paths) == 1) {
        if (missing(filter_var)) {
            geom <- read_shp_points(shp_paths, crs = crs)
        } else {
            geom <- read_shp_points(shp_paths, crs = crs, {{filter_var}},
                filter_values)
        }
    } else {
        if (missing(filter_var)) {
            geom <- purrr::map(shp_paths, read_shp_points, crs = crs) %>%
                purrr::reduce(sf::st_union)
        } else {
            geom <- purrr::map(shp_paths, read_shp_points, crs = crs,
                filter_var = {{filter_var}}, filter_values = filter_values) %>%
                purrr::reduce(sf::st_union)
        }
    }
    geom
}

read_shp_points <- function(shp_path, crs, filter_var, filter_values) {
    geom <- sf::read_sf(shp_path)
    if (!missing(filter_var)) {
        geom <- geom %>%
            dplyr::filter({{filter_var}} %in% filter_values)
    }
    geom <- geom %>%
        sf::st_transform(crs)
    geom
}

