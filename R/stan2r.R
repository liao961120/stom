p_sc = c( "alpha", "beta" )
p_1d = c( "x[1]", "x[2]" )
p_2d = c("y[1,1]", "y[2,1]", "y[1,2]", "y[2,2]")
p_3d = c("z[1,1,1]", "z[2,1,1]", "z[1,2,1]", "z[2,2,1]", "z[1,1,2]", "z[2,1,2]", "z[1,2,2]", "z[2,2,2]")

params = c( p_sc, p_1d, p_2d, p_3d ) |> sample()
vals = sapply( params, function(x) paste0("'",x,"'") )


# ToDo: separate params/vals parsing in parse_stan_params
#       param names need only be parsed once, vals can
#       be repeatedly fed in after the param names are parsed
x = parse_stan_params(params, vals)
init_array( x$array[names(x$array) == "y"] )
# get_dim_info(x$array$z$val)
# x$array[names(x$array) == "x"]


pat_bracket = "([a-zA-Z][a-zA-Z0-9_-]*)\\[([^a-zA-Z]+)\\]$"

parse_stan_params = function( params, vals ) {
    idx_nd = grepl( pat_bracket, params )
    # scalar parameters
    scalar = params[!idx_nd]
    scalar_vals = vals[!idx_nd]
    # names(scalar_vals) = scalar
    out_scalar = lapply( seq_along(scalar), function(i) {
        list(
            pos = 1,
            val = scalar_vals[i]
        )
    })
    names(out_scalar) = scalar

    # vector, matrix, array parameters
    multi_dim = params[idx_nd]
    multi_dim_vals = vals[idx_nd]

    symb = gsub( pat_bracket, "\\1", multi_dim )
    pos  = gsub( pat_bracket, "\\2", multi_dim )
    pos = strsplit( pos, ",", fixed=T )
    pos = lapply( pos, function(x) as.integer(x) )
    out_nd = lapply( seq_along(multi_dim), function(i) {
        list(
            pos = pos[[i]],  # vector
            val = multi_dim_vals[i]
        )
    })
    names(out_nd) = symb

    list(
        scalar = out_scalar,
        array = out_nd
    )
}


get_dim_info = function(lst) {
    mat = get_assign_positions(lst)
    if ( !is.matrix(mat) ) return(max(mat))
    apply( mat, 1, function(r) max(r) )
}

get_assign_positions = function(lst) {
    sapply( lst, function(e) e$pos )
}


init_array = function( lst ) {
    dim_ = get_dim_info(lst)
    vals = sapply( lst, function(e) e$val )
    # return(dim_)

    # scalar
    if ( length(dim_) == 1 && dim_ == 1 )
        return( vals )
    #return( vector("numeric", 1) )

    # vector
    if ( length(dim_) == 1 && dim_ > 1 ) {
        cat("assign vector")
        pos = get_assign_positions(lst)
        a = vector("numeric", dim_)
        for ( i in seq_along(pos) )
            assign_array("a", pos[i], vals[i] )
        return(a)
    }

    # matrix
    if ( length(dim_) == 2 ) {
        a = matrix(data = NA, nrow=dim_[1], ncol=dim_[2])
        pos = get_assign_positions(lst)
        for ( i in 1:ncol(pos) )
            assign_array("a", pos[,i], vals[i] )
        return(a)
        # return( matrix(data = NA, nrow=dim_[1], ncol=dim_[2]) )
    }

    # array
    if ( length(dim_) > 2 ) {
        a = array( data = NA, dim=dim_ )
        pos = get_assign_positions(lst)
        for ( i in 1:ncol(pos) )
            assign_array("a", pos[,i], vals[i] )
        return(a)
    }
}



# idx = c( 1, 2, 2 )
# z = array(NA, dim=c(3,3,2))
# assign_array("z", idx, 5)
assign_array = function(arr, idx, val) {
    idx = paste(idx, collapse="," )
    expr = paste0( arr, "[", idx, "] <- ", val )
    eval( parse(text=expr), envir = parent.frame() )
}

