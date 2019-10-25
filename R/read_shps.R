#' Reads shapefiles, projects to given crs and returns polygon simple feature
#'
#' @details Recieves path(s) to shapefile(s), projects to given crs and returns
#' a single simple feature.
#' @param shp_paths vector of path to shape files (or single path)
#' @param crs string specifying projection (proj4string) passed to
#' sf::st_transform, defaults to lcc (Mex)
#' @param top in case of multipolygon geometry in one of the single shapefiles,
#' TRUE indicates that only the biggest polygon will be included, if FALSE all
#' polygons will be included
#' @export
read_shps <- function(shp_paths, crs = "lcc_mex", top = TRUE) {
    if (crs == "lcc_mex") {
        crs <- "+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +datum=WGS84 +units=m +no_defs"
    }
    if (length(shp_paths) == 1) {
        geom <- read_shp(shp_paths, crs = crs, top = top)
    } else {
        geom <- purrr::map(shp_paths, read_shp, crs = crs, top = top) %>%
            purrr::reduce(sf::st_union)
    }
    geom
}
read_shp <- function(shp_path, crs, top) {
    geom <- sf::read_sf(shp_path)
    if (sf::st_geometry_type(geom) != "POLYGON") {
        geom <- geom %>%
            sf::st_cast("POLYGON")
        if (top) {
            geom <- geom %>%
                dplyr::mutate(area = sf::st_area(.)) %>%
                dplyr::top_n(1, area)
        }
    }
    if (sf::st_crs(geom)$proj4string != crs) {
        sf::st_transform(geom, crs)
    }
    geom
}

