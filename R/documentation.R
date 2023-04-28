#' Extract docstring from R script to various output formats
#'
#' The docstring is defined as lines that start with `#'`. To avoid problems
#' when converting to `.pdf` and `.html`, the format of documentation should
#' comply to Pandoc's markdown syntax.
#' To allow for the execution of  `export_docs_html()` and `export_docs_pdf()`,
#' pandoc needs to be available through the command line. In addition, a LaTeX
#' distribution (e.g. TeXLive) is also required for PDF compilation.
#'
#' @param path Path to input R script.
#' @param outfp Output file path to write to.
#' @param style String. Predefined style to apply to PDF outputs. Currently,
#'        only `amsart` is available.
#' @export
#' @examples
#' (fin = system.file("cases", "wine_network", "wine.R", package = "stom"))
#' docstring = export_docs(fin)  # Return as character vector if `outfp=NULL`
#' cat(docstring, sep="\n")
#' \dontrun{
#' export_docs(fin, "docs.md")
#' export_docs_pdf(fin, "docs.pdf")
#' export_docs_html(fin, "docs.html")
#' }
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
  pandoc_pdf(fin, outfp, style)
}


#' @rdname export_docs
#' @export
export_docs_html = function(path, outfp) {
  fin = tempfile()
  export_docs(path, outfp=fin)
  pandoc_html(fin, outfp)
}



#' Pandoc wrappers for converting markdown to pre-styled documents
#'
#' @export
pandoc_pdf = function(fin, outfp, style="amsart") {
  pandoc( fin,
          "--shift-heading-level-by=-1",
          "--from=markdown+tex_math_dollars",
          "--to=pdf", "--pdf-engine=xelatex",
          get_pandoc_pdf_args(style),
          "-o", outfp )
}

#' @rdname pandoc_pdf
#' @export
pandoc_html = function(fin, outfp, style=NULL) {
  if (Sys.info()['sysname'] == "Windows")
    Sys.setlocale("LC_TIME", "C")
  pandoc( fin,
          "--from=markdown+tex_math_dollars",
          "--to=html5",
          "--katex",
          "-A", system.file("template", "after-body.html", package="stom"),
          "-c", system.file("template", "pandoc.css", package="stom"),
          paste0("-V date=", '"', format.Date(Sys.Date(),"%B %d, %Y"), '"'),
          "-s",
          "-o", outfp )
}


get_pandoc_pdf_args = function(style="") {
  if ( style == "amsart" ) {
    # Before body
    before_body = tempfile()
    writeLines("\\vspace{-15pt}\\footnotesize\\begin{center}\\today\\end{center}\\vspace{12pt}\n",
               before_body)
    return(c(
      "-B", before_body,
      "--number-sections",
      "--variable=documentclass:amsart",
      "--variable=classoption:reqno",
      "--variable=classoption:12pt",
      "--variable=geometry:left=1in",
      "--variable=geometry:top=1in",
      "--variable=geometry:headheight=0.25in",
      "--variable=geometry:headsep=0.4in",
      "--variable=geometry:footskip=0.4in",
      "--variable=geometry:paperwidth=7in",
      "--variable=geometry:paperheight=10in",
      "--variable=geometry:text={5in,8in}"))
  }
  return(c())
}




pandoc = function(...) {
  pd = ifelse( Sys.info()['sysname'] == "Windows", "pandoc.exe", "pandoc" )
  args = c(...)
  cat( "  ...Executing system command...\n" )
  cat( pd, args, "\n\n" )
  system2( pd, args=args, stdout=FALSE )
}

