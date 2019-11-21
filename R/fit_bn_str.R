#' If structure not given, learn a network structure and fits BN
#'
#' @param bn_data data.frame with only categorical columns
#' @param bn_str optional bayesian network structure, if missing will be learned
#' @param score used for structure learning algorthm (hill-climbing)
fit_bn_str <- function(bn_data, bn_str, score = "k2") {
    bn_data <- as.data.frame(bn_data)
    if (missing(bn_str)) {
        bn_str <- bnlearn::hc(bn_data, score = score)
    }
    fit <- bnlearn::bn.fit(bn_str, bn_data, method = 'bayes', iss = 4)
    list(fit = fit, bn_str = bn_str)
}
