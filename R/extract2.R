# Posterior samples extraction

#' R6 representation of posterior samples
#'
#' @param fit  A `CmdStanFit` object passed to `stom::extract()`.
#' @param sim_params List. A list of simulated parameter values. See argument
#'        `fit` of `stom::get_param_info()`.
#' @param trim Whether to trim off parameters not present in `sim_params` from
#'        the posterior samples retrieved from `CmdStanFit`. By default, TRUE.
#' @return A `R6` `postParams` object.
#' @export
#' @examples
#' fp_sim = system.file("cases", "treatment_dynamics", "2-varying-effect-model", "simulation.R", package="stom")
#' fp_mod = system.file("cases", "treatment_dynamics", "2-varying-effect-model", "m1-wider_Item_difficulty.RDS", package="stom")
#' source(fp_sim)
#' d = sim_data()       # Simulated data
#' m = readRDS(fp_mod)  # CmdStanFit model cache
#' post = extract2(m, d$params)
extract2 = function(fit, sim_params, trim=T) {
    postParams = R6::R6Class("postParams",
                             lock_objects=FALSE,
                             public = list(
                                 initialize = function( post, par_info ) {
                                     self$.post = post
                                     self$.par_info = par_info
                                     self$.cache = list()
                                 },
                                 add_function = function(name, meth) {
                                     self[[name]] <- meth
                                     environment(self[[name]]) <- environment(self$add_function)
                                 },
                                 get_param = function(p) {
                                     if (self$.par_info[[p]]$cls == "vector" && self$.par_info[[p]]$dim == 1)
                                         return( self$.post[[p]] )
                                     if ( p %in% names(self$.cache) )
                                         return( self$.cache[[p]] )
                                     # First query: cache results
                                     self$.cache[[p]] = as_posterior_array(self$.post, p)
                                     return( self$.cache[[p]] )
                                 }
                             )
    )

    par_info = get_param_info( sim_params )
    if (trim)
        post = stom::extract( fit, pars=names(par_info) )
    else
        post = stom::extract( fit )
    post_samples = postParams$new( post=post, par_info=par_info )
    for ( p in names(par_info) ){
        post_samples$add_function(p, function(idx=NULL) {
            if ( is.null(idx) )
                return( self$get_param(p) )
            return( self$get_param(p)[idx] )
        })
    }
    return(post_samples)
}


#' Get parameter info from simulation
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
#' fp = system.file("cases", "treatment_dynamics", "2-varying-effect-model", "simulation.R", package="stom")
#' source(fp)
#' d = sim_data()
#' str(d$params)
#' get_param_info(d$params)
get_param_info = function(lst) {
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
