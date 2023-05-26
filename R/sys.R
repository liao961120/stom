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



#' Normalize to Unix paths
#'
#' @param x Character. Paths.
#' @return Character
#' @export
#' @examples
#' fp = 'C:\\Users\\rd\\bin\\export_docs_pdf.R'
#' to_unix_path(fp)
#' to_gitbash_path(fp)
#' get_cli_programs(F) |> print_paths()
to_unix_path = function(x) normalizePath(x, winslash = "/")


#' @rdname to_unix_path
#' @export
to_gitbash_path = function(x) {
    x = to_unix_path(x)
    x = gsub("^([A-Za-z]):", "/\\1", x)
    x
}


#' @rdname to_unix_path
#' @export
print_paths = function(x, win=T) {
    if (win) x = normalizePath(x)
    cat(x, sep="\n")
}



#' Copy to and from clipboard
#'
#' @param x Character.
#' @export
#' @examples
#' fp = 'C:\\Users\\rd\\bin\\export_docs_pdf.R'
#' fp |> xclip()  # copy to clipboard
#' fclip() |> cat()
xclip = function(x) clipr::write_clip(x)

#' @rdname xclip
#' @export
fclip = function() clipr::read_clip()

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
