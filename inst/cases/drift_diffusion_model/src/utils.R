library(stom)
N_CORES = 4

# Initialization for stan fitting
init_files_one_subj = function(dir="init_one_subj",
                            chains=1:N_CORES,
                            seed=50) {
    if (exists(".Random.seed")) {
        old <- .Random.seed
        on.exit( { .Random.seed <<- old } )
    }
    set.seed(seed)
    if (! dir.exists(dir) ) dir.create(dir, recursive = T)

    sapply( chains, function(chain) {
        init = tibble::lst(
            alpha = runif(1, .8, 2),
            beta  = runif(1, .3,.7),
            tau   = runif(1, .1,.3),
            dr1   = rtnorm(1, 1, 1),
            dr2   = -rtnorm(1, 1, 1)
        )
        fp = file.path(dir, paste0(chain,".json") )
        cmdstanr::write_stan_json(init, fp)
        fp
    })
}


init_files_multi_subj = function(dir="init_multi_subj",
                               chains=1:N_CORES,
                               seed=50) {
    if (exists(".Random.seed")) {
        old <- .Random.seed
        on.exit( { .Random.seed <<- old } )
    }
    set.seed(seed)
    if (! dir.exists(dir) ) dir.create(dir, recursive = T)

    sapply( chains, function(chain) {
        init = tibble::lst(
            # To Do: construct stan program and write params here
        )
        fp = file.path(dir, paste0(chain,".json") )
        cmdstanr::write_stan_json(init, fp)
        fp
    })
}


# Get posterior summaries
get_post_summ = function(d_precis, params, stat="mean") {
    summ = lapply(params, \(x) erect(d_precis, x, stat))
    names(summ) = params
    return(summ)
}
