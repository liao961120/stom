# Functions for working with stan/cmdstanr

#' Compile and sample with cmdstanr
#'
#' @param stan_file String. Stan file path.
#' @param data List. Data list.
#' @param seed Integer. Random seed passed to cmdstanr's `$sample()`.
#' @return A `CmdStanFit` object.
#' @export
stan = function(stan_file, data, seed=123) {
  m = cmdstanr::cmdstan_model(stan_file)
  # Fit object
  m$sample(
    data = data,
    refresh = 500,
    seed = seed
  )
}


#' Posterior summary and extraction from CmdStanFit objects
#'
#' @param fit  A `CmdStanFit` object.
#' @param depth Integer. The level of parameters to display. `0` for scalar;
#'        `1` for scalar and vector; `2` for scalar, vector, and matrix; and
#'        so on.
#' @param pars Character vector. Names of the variables to include.
#'        Passed to the `variables` argument in cmdstanr's
#'        `$summary()`/`$draws()`.
#' @param lp Boolean. Whether to include log probabilities in output.
#' @return A data frame
#' @export
#' @examples
#' # Fitted model
#' fp = system.file("cases", "wine_network", "wine2_normal_first_level2.RDS", package="stom")
#' m = readRDS(fp)
#'
#' # Posterior distribution summary
#' precis( m, depth=2, pars=c("Int", "J") )
#'
#' # Extract posterior samples
#' post = extract(m)
#' str(post)
precis = function(fit, depth=1, pars=NULL, lp=F) {
  d = fit$summary(pars, "mean", "sd", "quantile2", "rhat", "ess_bulk")
  if (!lp)
    d = d[ d$variable != "lp__", ]
  idx_vars = grepl( pat_depth(depth), d$variable )
  d = d[idx_vars, ]
  return( as.data.frame(d) )
}


#' @rdname precis
#' @export
extract = function(fit, pars=NULL, lp=F) {
  d = fit$draws(pars, format = "draws_df")
  if (!lp)
    d = d[ , colnames(d) != "lp__" ]
  return(d)
}


#' Test cmdstanr compilation
#'
#' @param clean Whether to recompile if binary found, defaults to FALSE.
#' @param draw_sample Whether to draw samples, defaults to TRUE.
#' @export
test_cmdstanr = function(clean=F, draw_sample=T) {
  fp = system.file("stan", "test.stan", package = "stom")
  if (clean) {
    bin = stan2bin(fp)
    if (file.exists(bin)) file.remove(bin)
  }
  m = cmdstanr::cmdstan_model(fp)
  if (draw_sample)
    return(
      m$sample( data=list(y=rnorm(1000),N=1000), refresh = 500 )
    )
  m
}


#' Convert stan file path to stan binary file path
#'
#' Assumes `*.stan` & compiled binary found in same directory.
#'
#' @param fp Path to `*.stan` file
stan2bin = function(fp) replace_file_ext(fp, bin_ext())


#' Replace file extension
#'
#' @param fp Character vector of file paths.
#' @param ext String. Extension (with dot) to change to.
#' @examples
#' replace_file_ext("x/y/zcda.txt")
#' replace_file_ext(paste0(1:3, ".txt"), ".stan")
#' @export
replace_file_ext = function(fp, ext="") {
  no_ext = tools::file_path_sans_ext(fp)
  return( paste0(no_ext, ext) )
}


#' Returns binary extension depending on OS
#'
#' ToDo: check mac's binary extension
bin_ext = function() {
  if (Sys.info()["sysname"] == "Windows")
    return(".exe")
  return("")
}







#' Stan parameter regex pattern
#'
#' @param depth Integer. The parameters' dimension number. Parameters with
#'        dimension number equal to or less than `depth` will be retrieved.
#'
#' @examples
#' pat_depth(0)
#' pat_depth(1)
#' pat_depth(2)
#' grepl( pat_depth(0), c("a", "a[1]", "a[1,2]", "a[1,2,3]") )
#' grepl( pat_depth(2), c("a", "a[1]", "a[1,2]", "a[1,2,3]") )
pat_depth = function(depth=1) {
  pats = sapply( 0:depth, function(i) pat_depth_atom(i) )
  paste( pats, collapse="|" )
}

pat_depth_atom = function(depth=1) {
  if (depth == 0) return("[a-zA-Z0-9_]$")
  pre_ = "[a-zA-Z0-9_]\\["
  suf_ = "\\]$"
  mid = paste( rep("\\d+", depth), collapse = "," )
  pat = paste0( pre_, mid, suf_ )
  return(pat)
}






