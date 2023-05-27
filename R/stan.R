# Functions for working with stan/cmdstanr

#' Compile and sample with cmdstanr
#'
#' @param stan_file String. Stan file path.
#' @param data List. Data list.
#' @param seed Integer. Random seed passed to cmdstanr's `$sample()`.
#' @return A `CmdStanFit` object.
#' @export
stan = function(stan_file, data, refresh = 250,...) {
  m = cmdstanr::cmdstan_model(stan_file)
  # Fit object
  m$sample(
    data = data,
    refresh = refresh,
    ...
  )
}


#' Save `CmdStanFit` object as RDS with a cross-platform consistent name
#'
#' @param m A `CmdStanFit` object returned from `stom::stan()`.
#' @export
save_model = function(m, fp=NULL) {
    if ( is.null(fp) ) {
        fp = basename(m$runset$args$exe_file)
        fp = stom::replace_file_ext(fp, ".RDS")
    }
    m$save_object(fp)
    cat("CmdStanFit Object saved to", fp, "\n")
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
#' precis( m, depth=2, pars="Int,J" )  # or pars=c("Int", "J")
#'
#' # Extract posterior samples
#' post = extract(m)
#' str(post)
precis = function(fit, depth=1, pars=NULL, lp=F) {
  pars = parse_pars( pars )
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
  pars = parse_pars( pars )
  d = fit$draws(pars, format = "draws_df")
  if (!lp)
    d = d[ , colnames(d) != "lp__" ]
  return(d)
}


#' Select/Filter parameters from precis/extract data frames
#'
#' @param d data frame. A data frame returned by `stom::precis()` or
#'        `stom::extract()`.
#' @param pars Character vector of parameter names to retrieve.
#' @return A subset of data frame with given parameters.
#' @export
#' @examples
#' fp = system.file("cases", "wine_network", "wine2_normal_first_level2.RDS", package="stom")
#' m = readRDS(fp)
#' d = precis( m, depth=3 )
#' get_pars( d, c("Int", "Int_raw") )
#'
#' d = extract( m )
#' get_pars( d, "Int, Int_raw" )
get_pars = function(d, pars) {
    pars = parse_pars(pars)
    pat = pat_param(pars)
    # precis data frame
    if ("variable" %in% names(d)) {
        suppressWarnings({
            d = d[grepl(pat, d$variable), ]
        })
    } else {
        # posterior sample data frame
        idx_col = grepl(pat, names(d))
        suppressWarnings({
            d = d[, idx_col]
        })
    }
    d
}

#' Convert posterior data frame to a list of array
#'
#' @param d Data frame. Posterior samples returned from `stom::extract()`
#'        or `stom::get_pars()`.
#' @param param String. Name of the parameter to extract (without brackets).
#'        By default, `NULL`, which assumes the input data frame `d` contains
#'        only the columns of the parameter of interest.
#' @param sample Integer. Number of samples to draw from the posterior samples.
#'        If `NULL`, returns the full data frame of the posterior samples.
#' @return A list of arrays, with each array corresponding to a draw from the
#'         posterior samples (i.e. a row in the posterior data frame).
#' @export
#' @examples
#' fp = system.file("cases", "wine_network", "wine2_normal_first_level2.RDS", package="stom")
#' m = readRDS(fp)
#' post = extract(m)
#' as_posterior_array( post, param="Int", sample_n=5 )
as_posterior_array = function(d, param=NULL, sample_n=NULL) {
    if (!is.null(param))
        d = get_pars(d, pars=param)
    row_idx = seq_along(d[[1]])
    if (!is.null(sample_n) && sample_n > 0)
        row_idx = sample(row_idx, size = sample_n, replace = F)
    vars = colnames(d)
    dim_ = get_array_size(vars)
    lapply( row_idx, function(i) {
        arr = array( unlist(d[i,]), dim=dim_ )
    })
}


# x = c("zE[1,1]","zE[2,1]","zE[1,2]","zE[2,2]")
# bracket_to_array(x, x)
bracket_to_array = function(key, val) {
    dim_ = get_array_size(key)
    # Fill in values (stan's param order corresponds to R's array fill-in order)
    arr = array( val, dim=dim_ )
    arr
}

get_array_size = function(key) {
    size_info = split_bracket_expression(key)

    # Check
    if ( any(names(size_info) != names(size_info)[1]) )
        stop( "Multiple parameters found!" )
    dim_len = sapply( size_info, function(x) length(x) )
    if ( any( dim_len != dim_len[1] ) )
        stop( "Multiple dimensions found!" )

    # Size up array
    size_info_flat = sapply(size_info, function(x) x)
    if ( is.null(dim(size_info_flat)) ) {
        dim_ = max(size_info_flat)
    } else {
        dim_ = apply( size_info_flat, 1, function(r) max(r) )
    }
    return(dim_)
}


# x = c("zE[1,1]","zE[2,1]","zE[1,2]","zE[2,2]")
# size_info = split_bracket_expression(x)
split_bracket_expression = function(x) {
    pat = "([^\\[\\]+)\\[(\\d+,?\\d*)\\]$"
    #gsub(pat, "\\2",  c("zE[1,1]","zE[1]") )
    letters = gsub(pat, "\\1", x)
    digits = gsub(pat, "\\2", x)
    digits = lapply( digits,
                     function(x) as.integer( strsplit(x, ",", fixed=T)[[1]] )
    )
    names(digits) = letters
    return(digits)
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

pat_param = function(x) {
    if (endsWith(x, "]")) {
        x = gsub( "[", "\\[", x, fixed=T )
        x = gsub( "]", "\\]", x, fixed=T )
        return(x)
    }
    digits = "(\\d+,?)+"
    x0 = paste0( "^", x, "$" )
    x = paste0( "^", x, "\\[", digits, "\\]$" )
    # if (length(x) > 1)
    x = c(x0, x)
    x = paste( x, collapse="|" )
    return(x)
}


parse_pars = function(x) {
  x = as.character(x)
  if (length(x) == 1 && !grepl("\\d+,\\d+", x) )
    return( strsplit(x, ", ?")[[1]] )
  x
}
