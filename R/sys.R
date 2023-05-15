# System related utilities

#' Replace file extension
#'
#' @param fp Character vector of file paths.
#' @param ext String. Extension (with dot) to change to.
#' @examples
#' replace_file_ext("x/y/zcda.txt")
#' replace_file_ext(paste0(1:3, ".txt"), ".stan")
#' @export
replace_file_ext = function(fp, ext="") {
  no_ext = tools::file_path_sans_ext(fp)
  return( paste0(no_ext, ext) )
}


#' Returns binary extension depending on OS
bin_ext = function() {
  if (Sys.info()["sysname"] == "Windows")
    return(".exe")
  return("")
}


wait_for = function(what, refresh=3, fail_after=12) {
  cat("Waiting for creation of missing files:", what, "\n" )
  cat("Returns FALSE if files not found after", fail_after, "secs\n")
  time_elapsed = 0
  while ( TRUE ) {
    Sys.sleep(time = refresh)
    time_elapsed = time_elapsed + refresh
    cat("  Time elapsed:", time_elapsed, "secs\n")

    if ( all(file.exists(what)) || time_elapsed >= fail_after ) break
  }
  succeed = all(file.exists(what))
  return(succeed)
}
