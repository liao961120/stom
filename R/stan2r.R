#' Infer parameter data structure from Stan's parameter names
#'
#' This function is used in `stom::extract2()` for inferring parameter structure
#' from Stan model outputs.
#'
#' @param params Character. A vector of parameter names to pass in.
#' @return A list of parameters, matching their input order, with data structure
#'         information.
#' @examples
#' p_scalar = c( "alpha", "beta" )
#' p_1d = c( "x[1]", "x[2]" )
#' p_2d = c("y[1,1]", "y[2,1]", "y[1,2]", "y[2,2]")
#' p_3d = c("z[1,1,1]", "z[2,1,1]", "z[1,2,1]", "z[2,2,1]", "z[1,1,2]", "z[2,1,2]", "z[1,2,2]", "z[2,2,2]")
#' params = c( p_scalar, p_1d, p_2d, p_3d )
#' get_stan_param_info(params)
get_stan_param_info = function( params ) {
    pat_bracket = "([a-zA-Z][a-zA-Z0-9_-]*)\\[([^a-zA-Z]+)\\]$"
    is_array = grepl( pat_bracket, params )
    params_names = gsub( pat_bracket, "\\1", params )

    parsed_params = lapply(seq_along(params), function(i) {
        # Scalar
        if ( !is_array[i] ) {
            return(list(
                type = "scalar",
                pos = 1
            ))
        } else {
            pos  = gsub( pat_bracket, "\\2", params[i] )
            pos = strsplit( pos, ",", fixed=T )[[1]]
            pos = as.integer( pos )
            l = length(pos)
            tp = ifelse( l == 1, "vector",
                                 ifelse( l == 2, "matrix", "array")
                        )
            return(list(
                type = tp,
                pos = pos
            ))
        }
    })
    names(parsed_params) = params_names
    return(parsed_params)
}


get_dim_info = function(lst) {
    mat = get_assign_positions(lst)
    if ( !is.matrix(mat) ) return(max(mat))
    apply( mat, 1, function(r) max(r) )
}

get_assign_positions = function(lst) {
    sapply( lst, function(e) e$pos )
}


init_array = function( lst, vals ) {
    dim_ = get_dim_info(lst)
    # vals = sapply( lst, function(e) e$val )
    tp = lst[[1]]$type
    # return(dim_)

    # scalar
    if ( tp == "scalar" )
        return( vals )
    #return( vector("numeric", 1) )

    # vector
    if ( tp == "vector" ) {
        pos = get_assign_positions(lst)
        a = vector("numeric", dim_)
        for ( i in seq_along(pos) )
            assign_array("a", pos[i], vals[i] )
        return(a)
    }

    # matrix
    if ( tp == "matrix" ) {
        a = matrix(data = NA, nrow=dim_[1], ncol=dim_[2])
        pos = get_assign_positions(lst)
        for ( i in 1:ncol(pos) )
            assign_array("a", pos[,i], vals[i] )
        return(a)
        # return( matrix(data = NA, nrow=dim_[1], ncol=dim_[2]) )
    }

    # array
    a = array( data = NA, dim=dim_ )
    pos = get_assign_positions(lst)
    for ( i in 1:ncol(pos) )
        assign_array("a", pos[,i], vals[i] )
    return(a)
}



# idx = c( 1, 2, 2 )
# z = array(NA, dim=c(3,3,2))
# assign_array("z", idx, 5)
assign_array = function(arr, idx, val) {
    idx = paste(idx, collapse="," )
    expr = paste0( arr, "[", idx, "] <- ", val )
    eval( parse(text=expr), envir = parent.frame() )
}

