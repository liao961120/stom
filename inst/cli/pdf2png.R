VERSION = "0.0.1"

"Convert PDF to PNG(s) with mutool or inkscape.

Example:
    pdf2png input.pdf --page=1,3,5

Usage:
    pdf2png INPUT
    pdf2png INPUT [--dpi=DPI] [--white] [--page=PAGE] [-o OUTFILE]
    pdf2png INPUT [--dpi=DPI] [--white] [--page=PAGE] [--output=OUTFILE] [--inkscape]

Options:
    -h --help   Show this screen.
    --version   Show version.
    -o --output Specify output file path. If not given, inferred
                from input by changing the file extension.
    --dpi=DPI   Dots per inch for output PNGs [default: 300].
    --white     Add white background.
    --page=PAGE The pages to convert.
    --inkscape  Use inkscape as the default backend

Notes:
    To use this function on windows, inkscape v1.2.2 has to be installed.
    Versions above this do not work.
" -> DOC
if (!interactive()) {
    a = docopt::docopt(DOC, version = VERSION)
} else {
    a = docopt::docopt(DOC, args=c("xxx.pdf"), version = VERSION)
}
# print(a)  # View parsed command line arguments


# Set backend
if (system("which mutool", ignore.stdout=T, ignore.stderr=T) == 0) {
    backend = "mutool"
} else if (system("which inkscape", ignore.stdout=T, ignore.stderr=T)) {
    backend = "inkscape"
} else {
    stop("Neither `mutool` nor `inkscape` available from command line!")
}
if (a$inkscape) backend = "inkscape"

stom::pdf2png(a$INPUT, a$output, page=stom::as_vec(a$page), white=a$white, dpi=a$dpi, backend=backend)
