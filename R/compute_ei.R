#' Computes de Ecosystem Integrity Index for given a Bayesian Network
#'
#' @details Given a Bayesian Network and observed evidence (the path to a set
#' of rasters and a region specified by a polygon or set of polygons (simple
#' feature) it computes the distribution of the Ecological Itegirtiy node.
#' @param net list with bayesian network structure and fit, as returned by
#' fit_bn
#' @param ie_node character denoting the node corresponding to IE
#' @param ie_levels if IE is interval, character string determining the IE
#' levels
#' @param geom region sf, polygon(s) that determine analysis area
#' @param vars_paths paths to rasters with covariates to be included in BN
#' @param map logical value indicating if the MAP (maximum a posteriori)
#' class should be returned
#' @param crs string specifying projection (proj4string) passed to
#' sf::st_transform, defaults to lcc (Mex)
#' @value a list with a data.frame containing the coordinates, covariates and
#' IE score (and optionally the MAP) for each data point, and a raster with IE
#' scores.
#' @export
compute_ei <- function(net, ei_node = "zz_delt_vp", geom, vars_paths, ei_levels,
    map = FALSE, crs = "lcc_mex") {
    if (crs == "lcc_mex") {
        crs <- "+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +datum=WGS84 +units=m +no_defs"
    }

    # select only variables in markov blanket for faster computation
    mb_ei <- bnlearn::mb(net$bn_str, ei_node)
    vars_names <- basename(vars_paths) %>% tools::file_path_sans_ext()
    ind_mb <- (vars_names %in% mb_ei | stringr::str_detect(vars_names, "id"))

    # if ecological integrity is included in variable paths, the returning data
    # frame will include a column with the info for model evaluation
    ei_paths <- stringr::str_detect(vars_paths, ei_node)
    ei_in_paths <- purrr::has_element(ei_paths, TRUE)
    if (ei_in_paths) {
        vars_ei <- ei_paths | stringr::str_detect(vars_names, "id")
        vars_ei_df <- prep_geom_vars(vars_paths[vars_ei], geom, crs = crs)$vars_df %>%
            dplyr::select(-x, -y)
    }
    vars <- prep_geom_vars(vars_paths[ind_mb], geom, crs = crs)
    vars_bn <- dplyr::select_if(vars$vars_df, is.factor)
    vars_bn_distinct <- dplyr::distinct(vars_bn)

    # compile and predict
    net_fit <- net_ex$fit
    comp <- gRbase::compile(bnlearn::as.grain(net_fit))
    pred_ei_dist <- predict(comp, ei_node, newdata = vars_bn_distinct,
        type = "distribution")

    mat_pred <- pred_ei_dist$pred[[ei_node]][, ei_levels]
    ei_coef <- int_midpoint(ei_levels)
    ei_score <- mat_pred %*% ei_coef %>% as.numeric()
    ei_score_var <- as.numeric(mat_pred %*% (ei_coef ^ 2)) - (ei_score ^ 2)
    ei_class <- ei_levels[apply(mat_pred, 1, which.max)]

    vars_bn_pred <- vars_bn_distinct %>%
        tibble::add_column(ei_score = ei_score) %>%
        tibble::add_column(ei_score_var = ei_score_var) %>%
        tibble::add_column(ei_class_max = ei_class)

    if (map) {
        pred_ei_class <- predict(comp, ei_node, newdata = vars_bn_distinct,
            type = "class")
        vars_bn_pred <- vars_bn_pred %>%
            tibble::add_column(ei_class_map = pred_ei_class$pred[[ei_node]])
    }
    vars_bn_pred <- vars$vars_df %>%
        left_join(vars_bn_pred, by = mb_ei)

    if (ei_in_paths) {
        vars_bn_pred <- vars_bn_pred %>%
            left_join(vars_ei_df, by = "id")
    }
    vars_bn_ei <- vars_bn_pred %>% dplyr::select(ei_score, id)

    # return raster with ei, use raster_id as basis
    names(vars$raster_id) <- "id"
    raster_ei <- raster::subs(vars$raster_id, vars_bn_ei, by = "id",
        which = "ei_score", subsWithNA = TRUE) %>%
        raster::crop(y = geom) %>%
        raster::mask(mask = geom)
    list(ei_df = vars_bn_pred, raster_ei = raster_ei)
}
int_midpoint <- function(interval) {
    min_val <- stringr::str_extract(interval, "[0-9]+") %>%
        readr::parse_number()
    max_val <- stringr::str_extract(interval, ",[0-9]+") %>%
        readr::parse_number()
    (max_val + min_val) / 2
}
