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
export_docs_pdf = function(path, outfp) {
  fin = tempfile()
  export_docs(path, outfp=fin)
  pandoc = ifelse( Sys.info()['sysname'] == "Windows", "pandoc.exe", "pandoc" )
  system2(pandoc, args = c(fin,
                             "--from=markdown+tex_math_dollars",
                             "--to=pdf",
                             "-o",
                             outfp)
  )
}
