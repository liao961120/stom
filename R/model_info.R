#' Get number of samples (chains combined) from CmdStanFit
#'
#' @param fit CmdStanFit object.
#' @return Integer
#' @export
n_samples = function(fit)
    fit$metadata()$iter_sampling * fit$num_chains()
