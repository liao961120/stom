library(stom)
fp_sim = system.file("cases", "treatment_dynamics", "2-varying-effect-model", "simulation.R", package="stom")
fp_mod = system.file("cases", "treatment_dynamics", "2-varying-effect-model", "m1-wider_Item_difficulty.RDS", package="stom")

source("simulation.R")
d = sim_data()





str(d$params)


# make_store = function() {
#     STORE = 0
#     update_store = function(num=0) {
#         STORE <<- STORE + num
#         STORE
#     }
#     update_store
# }


array_type = function(x) {
    if ( is.matrix(x) ) return("matrix")
    if ( is.array(x) && length(dim(x)) == 2 ) return("matrix")
    if ( is.array(x) && length(dim(x)) > 2 ) return("array")
    "vector"
}

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
pars = get_param_info(d$params)




s = stom::precis( m, 3, pars = names(pars) )
post = stom::extract(m, pars = names(pars) )


library(R6)
postParams <- R6Class("postParams",
                    lock_objects=FALSE,
                    public = list(
                        initialize = function(post, par_info) {
                            self$.post = post
                            self$.par_info = get_param_info(par_info)
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
post_samples = postParams$new( post=post, par_info=d$params )
for ( p in names(pars) ){
    post_samples$add_function(p, function(idx=NULL) {
        if ( is.null(idx) )
            return( self$get_param(p) )
        return( self$get_param(p)[idx] )
    })
}

# get_post_param = function( pars ) {
#     
# }


post2 = lapply( names(pars), function(p) {
    if ( pars[[p]]$cls == "vector" && pars[[p]]$dim == 1 )
        return( post[[p]] )
    return( as_posterior_array(post, p) )
})
names(post2) = names(pars)



parse_simulated_params = function(x) {
    
}