#' PDF to image conversion
#'
#' Convert a PDF file to images with Inkscape. To use this function, `inkscape`
#' has to be available from the command line. Alternatively, users can specify
#' the absolute path to inkscape by the option `stom.inkscape` (e.g.,
#' `options(stom.inkscape = "/home/stom/bin/inkscape")`).
#'
#' @param infile String. Path to input PDF file.
#' @param dpi Integer. "Dots Per Inch" for PNG outputs with `pdf2png()`.
#' @param outfile Character vector. Output file paths without file extensions.
#'        File extensions will be set to `.png` for `pdf2png()` and `.svg` for
#'        `pdf2svg()`. The length of `outfile` should be `1` or corresponds to
#'        the length of `page`. If there is a mismatch, only the first element
#'        in `outfile` will be taken as the file stem, and the page numbers will
#'        be appended to the file stem as suffixes. By default, `outfile`
#'        is set to `NULL`, in which the input PDF file stem is used.
#' @param page Integer vector. The pages (starting with 1) to extract from the
#'        PDF file, defaults to `NULL`, in which every page is converted to
#'        an image.
#'
#' @examples
#' \dontrun{
#' infile = system.file("inkscape", "dag.pdf", package = "stom")
#' pdf2svg( infile, outfile="dag.svg" )
#' pdf2png( infile, dpi=800, outfile="dag.png" )
#' }
#' @export
pdf2png = function(infile, dpi=1200, outfile=NULL, page=NULL) {
  base_cmd = c(
    "--export-background-opacity=0",
    "--export-type=png",
    "--export-text-to-path",
    "--export-area-drawing",
    "--pdf-poppler",
    paste0("--export-dpi=", dpi)
  )
  # Detect PDF page number
  out_info = inkscape_pdf_pager(infile, ".png", outfile=outfile, page=page)
  # Execute command page by page
  inkscape_run_through_pages(infile, base_cmd, out_info)
}


#' @export
#' @rdname pdf2png
pdf2svg = function(infile, outfile=NULL, page=NULL) {
  base_cmd = c(
    "--export-background-opacity=0",
    "--export-type=svg",
    "--export-text-to-path",
    "--export-area-drawing",
    "--pdf-poppler"
  )
  # Detect PDF page number
  out_info = inkscape_pdf_pager(infile, ".svg", outfile=outfile, page=page)
  # Execute command page by page
  inkscape_run_through_pages(infile, base_cmd, out_info)
}


#' Run Inkscape through pages of PDF input, where page info is given by the
#' `out_info` arg obtained through `inkscape_pdf_pager()`
inkscape_run_through_pages = function(infile, base_cmd, out_info) {
  outfile = out_info$outfile
  page = out_info$page
  # Execute command page by page
  for (i in seq_along(page)) {
    outfp = paste0("--export-filename=", outfile[i])
    pagenum = paste0("--pdf-page=", page[i])
    inkscape( c(base_cmd, outfp, pagenum, infile) )
  }
}


#' Construct output file paths based on input PDF page numbers
inkscape_pdf_pager = function(pdf_file, to_ext=".svg", outfile=NULL, page=NULL) {
  # Detect PDF page number
  if (is.null(page))
    page = seq( pdftools::pdf_info(pdf_file)$pages )

  # Output files
  outfile0 = outfile
  if (is.null(outfile0)) outfile0 = pdf_file
  outfile = replace_file_ext(outfile0, "")
  if ( length(outfile) != length(page) )
    outfile = paste( outfile[1], page, sep="_" )  # append pagenum suffixes
  outfile = replace_file_ext(outfile, to_ext)

  # Discard pagenum if only one page
  if (length(page) == 1)
    outfile =  replace_file_ext(outfile0, to_ext)

  return(list( outfile=outfile, page=page ))
}



inkscape = function(cmd) {
  # Executable path
  exe = getOption("stom.inkscape", "inkscape")
  cat( paste(c(exe, cmd)) , "\n")
  system2( c(exe, cmd) )
}


options(stom.inkscape.bin = "inkscape")


