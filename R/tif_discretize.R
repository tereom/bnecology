#' Read tif file and discretize values
#'
#' Recieves a path to a tif, reads the raster, crops and sets to given extent,
#' discretizes values and writes a new raster with the same name in given path
#' @param tif_path path to tif file that will be read and discretized
#' @param breaks number of breaks passed to arules::discretize
#' @param extent extent to which raster will be cropped and set
#' @param path_save path to new file, where the discretized raster will be
#' @param vals_na values to be set to NA
#' saved
#' @importFrom magrittr %>%
#' @export

tif_discretize <- function(tif_path, breaks = 4, extent, path_save,
    vals_na = -1) {
    raster_cropped <- raster::raster(tif_path) %>%
        raster::crop(extent) %>%
        raster::extend(extent)
    vals_raster <- raster::values(raster_cropped)
    raster::values(raster_cropped)[vals_raster %in% vals_na] <- NA
    range_vals <- range(raster::values(raster_cropped), na.rm = TRUE)
    if (range_vals[1] >= 0 & range_vals[2] <= 1) {
        raster::values(raster_cropped) <- arules::discretize(
            values(raster_cropped), breaks = seq(0, 1, 1/breaks),
            method = "fixed")
    } else {
        raster::values(raster_cropped) <- arules::discretize(
            raster::values(raster_cropped), breaks = breaks)
    }
    raster::writeRaster(raster_cropped, filename = path_save)
}
