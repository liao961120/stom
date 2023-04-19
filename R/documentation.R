###########################################################
## Functions for extracting documentation from R scripts ##
###########################################################

#' Extract docstring from R script
#'
#' @param path Path to input R script.
#' @param outfp Output file path to write to.
#' @export
export_docs = function(path, outfp=NULL) {
  x = readLines(path)
  x = ifelse( startsWith(x, "#'"),
              substring(x, 4), "__KANLB__" )
  x = x[x != "__KANLB__"]
  if (is.null(outfp)) return(x)
  writeLines(x, outfp)
}


#' @rdname export_docs
#' @export
export_docs_pdf = function(path, outfp, style="amsart") {
  fin = tempfile()
  export_docs(path, outfp=fin)
  args = c( fin,
            "--from=markdown+tex_math_dollars",
            "--to=pdf", "--pdf-engine=xelatex",
            get_pandoc_pdf_args(style),
            "-o", outfp )
  cat( pandoc, args, "\n")
  system2(pandoc, args=args, stdout=FALSE)
}


get_pandoc_pdf_args = function(style="") {
  if ( style == "amsart" )
    return(c(
      "--variable=documentclass:amsart",
      "--variable=classoption:reqno",
      "--variable=classoption:12pt",
      "--variable=geometry:left=1in",
      "--variable=geometry:top=1in",
      "--variable=geometry:headheight=0.25in",
      "--variable=geometry:headsep=0.4in",
      "--variable=geometry:footskip=0.4in"))
  return(c())
}


pandoc = function(...) {
  pd = ifelse( Sys.info()['sysname'] == "Windows", "pandoc.exe", "pandoc" )
  args = c(...)
  system2(pd, args=args )
}


