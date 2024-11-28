#' Vector graphics conversion
#'
#' Convert a PDF/SVG file to images of other formats with Inkscape.
#'
#' To use this function, `mutool` (v1.24.0) or `inkscape` (v1.2.2) has to be
#' available from the command line. Alternatively, users can specify the
#' absolute path to inkscape by the option `stom.inkscape`
#' (e.g., `options(stom.inkscape = "/home/stom/bin/inkscape")`).
#'
#' @param infile String. Path to input PDF file.
#' @param dpi Integer. "Dots Per Inch" for PNG outputs with `pdf2png()`.
#' @param outfile String. By default, `outfile` is set to `NULL`, in which the
#'        input PDF file stem is used.
#' @param page Integer vector. The pages (starting with 1) to extract from the
#'        PDF file, defaults to `NULL`, in which every page is converted to
#'        an image.
#' @param backend String. The tool for converting images from PDFs. Defaults to `"mutool"`.
#'        Currently supported options: `"mutool"` and `"inkscape"`
#'
#' @examples
#' \dontrun{
#' infile = system.file("inkscape", "dag.pdf", package = "stom")
#' pdf2svg( infile, outfile="dag.svg" )
#' pdf2png( infile, outfile="dag.png", dpi=800 )
#' svg2pdf( "dag.svg", "dag2.pdf" )
#' svg2png( "dag.svg", "dag2.png" )
#' }
#' @export
pdf2png = function(infile,
                   outfile = NULL,
                   page = NULL,
                   dpi = 400,
                   white = FALSE,
                   backend = c("mutool", "inkscape") ) {
    if (backend[1] == "inkscape")
        pdf2png_inkscape(infile, outfile, page, dpi, white)

    if (backend[1] == "mutool") {
        if (!is.null(outfile)) outfile = xfun::with_ext(outfile, "png")
        mutool_convert(infile, outfile, format = "png", page = page, dpi = dpi, white = white)
    }
}


#' @export
#' @rdname pdf2png
pdf2svg = function(infile,
                   outfile = NULL,
                   page = NULL,
                   white = FALSE,
                   backend = c("mutool", "inkscape")) {
    if (backend[1] == "inkscape")
        pdf2svg_inkscape(infile, outfile, page, white)

    if (backend[1] == "mutool") {
        if (!is.null(outfile)) outfile = xfun::with_ext(outfile, "svg")
        mutool_convert(infile, outfile, format = "svg", page = page, white = white)
    }

}


#' Convert PDF to images by mutool
#'
#' @param infile String. Path to input PDF file.
#' @param outfile String Output file path. Defaults to NULL, and is inferred from `infile`.
#' @param dpi Integer. "Dots Per Inch" for PNG outputs.
#' @param format String. Output format. Ignored when `outfile` is given.
#' @param page Integer vector. The pages (starting with 1) to extract from the
#'        PDF file, defaults to `NULL`, in which every page is converted to
#'        an image.
#' @export
mutool_convert = function(infile,
                          outfile = NULL,
                          format = c("png", "svg"),
                          page = NULL,
                          dpi = 300,
                          white = FALSE) {
    # Set output format
    if (!is.null(outfile)) {
        fmt = xfun::file_ext(outfile)[1]
        if (!fmt %in% format) {
            format = format[1]
        } else {
            format = fmt
        }
    } else {
        format = format[1]
    }

    # Set output directory & file name
    if (is.null(outfile)) {
        outdir = dirname(infile)
        outfn = basename(xfun::with_ext(infile, format))
    } else {
        outdir = dirname(outfile)
        outfn = basename(xfun::with_ext(outfile, format))[1]
    }

    # Set page
    if (is.null(page)) {
        page = "1-N"
    } else {
        page = paste(page, collapse=",")
    }

    # Convert pages to temp dir
    tmpdir = tempdir()
    opts = c(
        paste0("resolution=",dpi),
        ifelse(white, "alpha", "")
    )
    args = c(
        "convert", "-F", format, "-o", file.path(tmpdir,outfn),
        "-O", paste(opts, collapse=","),
        infile, page
    )
    # mutool convert -F png -o dir/.png -O resolution=300,alpha dag-dawid.pdf 1-2
    cat("\n  mutools", args, "\n")
    system2("mutool", args)

    # Copy files to output directory
    # Rename according to the number of files generated
    fps = msg = list.files(tmpdir, full.names=T, pattern=paste0(format,"$"))
    if (length(fps) == 1) {
        outfp = msg = file.path(outdir, outfn)
        file.copy(fps, outfp, overwrite=T)

    } else {
        file.copy(fps, outdir, overwrite=T)
    }
    file.remove(fps)
    message("\nOutput file(s):\n  ", paste(basename(msg), collapse=", ") )
}


pdf2png_inkscape = function(infile,
                       outfile = NULL,
                       page = NULL,
                       dpi = 1200,
                       white = FALSE) {
    base_cmd = c(
        "--export-background-opacity=0"[!white],
        "--export-background=white"[white],
        "--export-type=png",
        "--export-text-to-path",
        "--export-area-drawing",
        "--pdf-poppler",
        paste0("--export-dpi=", dpi)
    )
    # Detect PDF page number
    out_info = inkscape_pdf_pager(infile, ".png", outfile = outfile, page = page)
    # Execute command page by page
    inkscape_run_through_pages(infile, base_cmd, out_info)
}


pdf2svg_inkscape = function(infile,
                   outfile = NULL,
                   page = NULL,
                   white = FALSE) {
    base_cmd = c(
        "--export-background-opacity=0"[!white],
        "--export-background=white"[white],
        "--export-type=svg",
        "--export-text-to-path",
        "--export-area-drawing",
        "--pdf-poppler"
    )
    # Detect PDF page number
    out_info = inkscape_pdf_pager(infile, ".svg", outfile = outfile, page = page)
    # Execute command page by page
    inkscape_run_through_pages(infile, base_cmd, out_info)
}


#' @export
#' @rdname pdf2png
svg2pdf = function(infile, outfile = NULL) {
    base_cmd = c(
        "--export-background-opacity=0",
        "--export-type=pdf",
        "--export-text-to-path",
        "--export-area-drawing",
        "--pdf-poppler"
    )
    # Output file
    outfile0 = outfile
    if (is.null(outfile0))
        outfile0 = infile
    outfile = replace_file_ext(outfile0, ".pdf")
    outfile = paste0( "--export-filename=", outfile )
    inkscape( c(base_cmd, outfile, infile) )
}


#' @export
#' @rdname pdf2png
svg2png = function(infile, outfile = NULL, dpi = 1200) {
    base_cmd = c(
        "--export-background-opacity=0",
        "--export-type=png",
        "--export-text-to-path",
        "--export-area-drawing",
        "--pdf-poppler",
        paste0("--export-dpi=", dpi)
    )
    # Output file
    outfile0 = outfile
    if (is.null(outfile0))
        outfile0 = infile
    outfile = replace_file_ext(outfile0, ".png")
    outfile = paste0( "--export-filename=", outfile )
    inkscape( c(base_cmd, outfile, infile) )
}


############ HELPERS ##############
#' Run Inkscape through pages of PDF input, where page info is given by the
#' `out_info` arg obtained through `inkscape_pdf_pager()`
inkscape_run_through_pages = function(infile, base_cmd, out_info) {
    outfile = out_info$outfile
    page = out_info$page
    # Execute command page by page
    for (i in seq_along(page)) {
        outfp = paste0("--export-filename=", outfile[i])
        pagenum = paste0("--pdf-page=", page[i])
        inkscape(c(base_cmd, outfp, pagenum, infile))
    }
}


#' Construct output file paths based on input PDF page numbers
inkscape_pdf_pager = function(pdf_file,
                              to_ext = ".svg",
                              outfile = NULL,
                              page = NULL) {
    # Detect PDF page number
    if (is.null(page))
        page = seq(pdftools::pdf_info(pdf_file)$pages)

    # Output files
    outfile0 = outfile
    if (is.null(outfile0))
        outfile0 = pdf_file
    outfile = replace_file_ext(outfile0, "")
    if (length(outfile) != length(page))
        outfile = paste(outfile[1], page, sep = "_")  # append pagenum suffixes
    outfile = replace_file_ext(outfile, to_ext)

    # Discard pagenum if only one page
    if (length(page) == 1)
        outfile =  replace_file_ext(outfile0, to_ext)

    return(list(outfile = outfile, page = page))
}


inkscape = function(cmd) {
    exe = getOption("stom.inkscape", "inkscape")  # Executable path
    cmd = c(exe, cmd)
    cat( paste(cmd), "\n" )
    system2( cmd )
}
