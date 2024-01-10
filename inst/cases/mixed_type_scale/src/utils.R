library(stom)
N_CORES = 4

# Initialization for stan fitting
write_init_files = function(sim, dir="init",
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
            A     = rnorm(sim$N, 0, 2),
            S     = rnorm(sim$N, 0, 2),
            E     = rnorm(sim$Ni, 0, 2),
            kappa = sort(runif(sim$Np-1,-3,3))
        )
        fp = file.path(dir, paste0(chain,".json") )
        cmdstanr::write_stan_json(init, fp)
        fp
    })
}


# Initialization for stan fitting
write_init_files_partialpool = function(sim, dir="init",
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
            zA     = rnorm(sim$N),
            zS     = rnorm(sim$N),
            zE     = rnorm(sim$Ni),
            muA    = rnorm(1),
            muS    = rnorm(1),
            muE    = rnorm(1),
            sigmaA = rtnorm(1),
            sigmaS = rtnorm(1),
            sigmaE = rtnorm(1),
            kappa = sort(runif(sim$Np-1,-3,3))
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
