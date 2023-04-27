.onLoad = function(libname, pkgname) {
  op = options()
  op.stom = list(
    mc.cores = parallel::detectCores()  # max cores for cmdstanr
  )
  toset = !(names(op.stom) %in% names(op))
  if (any(toset)) options(op.stom[toset])

  invisible()
}
