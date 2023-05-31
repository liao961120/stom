# Utilities for handling simulation data structures

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


#' @examples
#' d = list(
#'     B_AE = 2,
#'     E = matrix(0, 2, 2),
#'     kappa = 1:5,
#'     subj = list(
#'         sigma_subj = 1:3,
#'         Rho = matrix(0, 2, 2),  # strip off attributes
#'         Alpha_TD = 1
#'     )
#' )
#' get_deepest_elem_in_lst(d, "Rho")
get_deepest_elem_in_lst = function(lst, name) {
    if ( name %in% names(lst) ) {
        lst = lst[[name]]
        if ( is.recursive(lst) )
            return( get_deepest_elem_in_lst(lst, name) )
        return(lst)
    }
    rslt = lapply( names(lst), function(n) {
        if ( is.recursive(lst[[n]]) )
            return( get_deepest_elem_in_lst(lst[[n]], name) )
    })
    rslt = rslt[ sapply(rslt, function(x) !is.null(x)) ]
    len = length(rslt)
    if ( len == 0 ) return(NULL)
    if ( len > 1 ) warning(len, " names found! Return first!")
    rslt[[1]]
}
