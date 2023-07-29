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
    temp = pdf_filter(fin)
    pandoc(
        temp,
        "--citeproc",
        "--shift-heading-level-by=-1",
        "--from=markdown+tex_math_dollars+raw_tex+raw_attribute",
        "--to=pdf",
        "--pdf-engine=xelatex",
        # "--pdf-engine=lualatex",
        # "--pdf-engine=pdflatex",
        get_pandoc_pdf_args(style),
        "-o",
        outfp
    )
}


#' @export
pandoc_tex = function(fin, outfp, style="amsart") {
    temp = pdf_filter(fin)
    pandoc(
        temp,
        "--citeproc",
        "--shift-heading-level-by=-1",
        "--from=markdown+tex_math_dollars+raw_tex+raw_attribute",
        "--to=latex",
        # "--pdf-engine=lualatex",
        # "--pdf-engine=pdflatex",
        get_pandoc_pdf_args(style),
        "-o",
        outfp
    )
}


#' @rdname pandoc_pdf
#' @export
pandoc_html = function(fin, outfp, style=NULL) {
  if (Sys.info()['sysname'] == "Windows")
    Sys.setlocale("LC_TIME", "C")
  pandoc( fin,
          "--citeproc",
          "--default-image-extension=svg",
          "--from=markdown+tex_math_dollars+raw_tex+raw_attribute",
          "--to=html5",
          "--katex",
          "-A", system.file("template", "after-body.html", package="stom"),
          "-c", system.file("template", "pandoc.css", package="stom"),
          "-V", paste0("date=", '"', format.Date(Sys.Date(),"%B %d, %Y"), '"'),
          "-s",
          "-o", outfp )
}


get_pandoc_pdf_args = function(style="") {
  if ( style == "amsart" ) {
    header = tempfile()
    writeLines(c(""), header )
    # Before body
    before_body = tempfile()
    writeLines(c(
        ""
      # "\\vspace{-8pt}\\footnotesize\\begin{center}\\today\\end{center}\\vspace{12pt}\n",
      # "
      # \\makeatletter
      # \\let\\origsection\\section
      # \\renewcommand\\section{\\@ifstar{\\starsection}{\\nostarsection}}
      #
      # \\newcommand\\nostarsection[1]
      # {\\sectionprelude\\origsection{#1}\\sectionpostlude}
      #
      # \\newcommand\\starsection[1]
      # {\\sectionprelude\\origsection*{#1}\\sectionpostlude}
      #
      # \\newcommand\\sectionprelude{%
      #   \\vspace{.9em}
      # }
      #
      # \\newcommand\\sectionpostlude{%
      #   \\vspace{.35em}
      # }
      # \\makeatother
      # "
      ),
      before_body
    )
    return(c(
      "-H", header,
      "-B", before_body,
      "--template", system.file("template", "default.latex", package = "stom"),
      "--number-sections",
      "--default-image-extension=pdf",
      "--variable=documentclass:amsart",
      "--variable=classoption:reqno,12pt",
      # Fonts
      "-V", "mathspec",
      '--variable=mainfont:"Adobe Caslon Pro"',
      "--variable=mainfontoptions:Scale=1.28,Numbers={Lining,Proportional}",
      '--variable=monofont:"Monego"',
      "--variable=monofontoptions:Scale=1",
      '--variable=mathfont:"Adobe Caslon Pro"',
      '--variable=greek-mathfont:"GFS Porson"',
      "--variable=mathfontoptions:Scale=1.28,Numbers={Lining,Proportional}",
      # "--variable=fontsize:12pt",
      ########
      # "--variable=linestretch:1.2",
      "--variable=indent:true",
      "--variable='geometry:paperwidth=7in,paperheight=10in,text={5in,8in},left=1in,top=1in,headheight=0.25in,headsep=0.4in,footskip=0.4in'"
      ))
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




pdf_filter = function(fin) {
  fout = tempfile()
  lines = readLines(fin)
  lines = ignore_dollars_with_begin_equation(lines)
  writeLines(lines, fout)
  fout
}

#' Spcial function to deal with conflicts between `$$` and equation env in latex
ignore_dollars_with_begin_equation = function(lines) {
  trim_lines = trimws(lines)
  idx_dollars = which( trim_lines == "$$" )
  idx_begin_eq = which( trim_lines == "\\begin{equation}" )
  idx_end_eq = which( trim_lines == "\\end{equation}" )
  idx_ignore = c()
  for ( i in idx_dollars ) {
    if ( (i+1) %in% idx_begin_eq )
      idx_ignore = c(idx_ignore, i)
    if ( (i-1) %in% idx_end_eq )
      idx_ignore = c(idx_ignore, i)
  }
  if ( length(idx_ignore) > 0 )
    return( lines[-idx_ignore] )
  lines
}

