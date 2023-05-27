# Posterior samples extraction

# See https://mc-stan.org/posterior/articles/rvar.html for similar
# implementation in package `posterior`
# extract3 = function(fit, pars=NULL, lp=F) {
#     pars = stom:::parse_pars( pars )
#     d = posterior::as_draws_rvars( fit$draws(format="draws_df") )
#     return(d)
# }

#' R6 representation of posterior samples
#'
#' `extract2()` provides an object-oriented interface to the posterior
#' samples extracted from a Stan model. In particular, high dimensional
#' parameters (i.e., vectors, matrices, and arrays) are gathered into arrays of
#' corresponding dimensions in R. These restructured high dimensional
#' parameters, as well as other scalar parameters, are exposed as public methods
#' in the returned R6 object. See below for an example.
#'
#' @param fit  A `CmdStanFit` object passed to `stom::extract()`.
#' @return An `R6`, `postParams` object.
#' @export
#' @examples
#' fp_mod = system.file("cases", "treatment_dynamics", "1-simplified-normal-model", "m1.RDS", package="stom")
#' m = readRDS(fp_mod)  # CmdStanFit model cache
#' post = extract2(m)
#' str(post)
#'
#' post$B_AD() |> str()
#' post$B_TE() |> str()
#' post$E() |> str()
extract2 = function(fit) {
    postParams = R6::R6Class("postParams",
                             lock_objects=FALSE,
                             public = list(
                                 initialize = function( post, par_info ) {
                                     private$.post = post
                                     private$.par_info = par_info
                                 }
                             ),
                             private = list(
                                 .cache = list(),
                                 add_function = function(name, meth) {
                                     self[[name]] <- meth
                                     environment(self[[name]]) <- environment(self$add_function)
                                 },
                                 get_param = function(p) {
                                     if ( p %in% names(private$.cache) )
                                         return( private$.cache[[p]] )
                                     if ( private$.par_info[[p]]$type == "scalar" )
                                         return( private$.post[[p]] )
                                     # First query: cache results
                                     private$.cache[[p]] = as_posterior_array( private$.post, param=p )
                                     return( private$.cache[[p]] )
                                 }
                             )
    )

    post = extract( fit, lp=T )
    par_info = get_stan_param_info( colnames(post) )
    post_samples = postParams$new( post=post, par_info=par_info )
    # Programmatically add parameter names as method (allow access as $param)
    for ( p in unique(names(par_info)) ){
        f = function( idx=NULL ) {
            fname = as.character( match.call()[[1]] )
            fname = strsplit(fname, "$", fixed = T)
            fname = rev(fname)[[1]]
            if ( is.null(idx) )
                return( private$get_param(fname) )
            return( private$get_param(fname)[idx] )
        }
        post_samples$.__enclos_env__$private$add_function(p, f)
    }
    return(post_samples)
}


#' Get parameter information of a simulation
#'
#' The input list can be nested in arbitrary levels, as long as the names at the
#' last level matches those used in the corresponding Stan model.
#'
#' @param lst List. A list of parameter values from the simulation. Refer
#'        to `system.file("cases", "treatment_dynamics", "2-varying-effect-model", "simulation.R", package="stom")`
#'        for the data structure of the input. It should match the structure of
#'        `sim_data()$params`.
#' @return A list of parameters with their dimension info.
#' @export
#' @examples
#' fp = system.file("cases", "treatment_dynamics", "1-simplified-normal-model", "simulation.R", package="stom")
#' source(fp)
#' d = sim_data()
#' str(d$params)
#' get_sim_param_info(d$params)
get_sim_param_info = function(lst) {
    paramInfo = make_info_store()
    get_param_info_recursive = function(lst, nm=NULL) {
        if ( !is.list(lst) ) {
            paramInfo(lst, nm)
            return(lst)
        }
        lst = lapply( names(lst), function(n) {
            get_param_info_recursive(lst[[n]], n)
        })
        paramInfo()
    }
    return( get_param_info_recursive(lst) )
}


#' Simplified array data type of R to match Stan's parameter dimensions
#' Vector: 1D;  matrix: 2D;  array: 3D or above
array_type = function(x) {
    if ( is.matrix(x) ) return("matrix")
    if ( is.array(x) && length(dim(x)) == 2 ) return("matrix")
    if ( is.array(x) && length(dim(x)) > 2 ) return("array")
    "vector"
}


# Create store during recursive parsing of nested parameter in get_param_info()
make_info_store = function() {
    STORE = list()
    update_store = function(obj=NULL, name=NULL) {
        if (is.null(obj)) return(STORE)
        l = length(STORE)
        cls = array_type(obj)
        DIM = dim(obj)
        if ( cls == "vector" )
            DIM = length(obj)
        STORE[[name]] <<- list(
            cls = cls,
            dim = DIM
        )
        STORE
    }
    update_store
}
