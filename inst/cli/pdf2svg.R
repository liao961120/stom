VERSION = "0.0.1"

"Convert PDF to SVG(s) with inkscape.

Example:
    pdf2svg input.pdf --page=1,3,5

Usage:
    pdf2svg INPUT
    pdf2svg INPUT [--white] [--page=PAGE] [-o OUTFILE]
    pdf2svg INPUT [--white] [--page=PAGE] [--output=OUTFILE] [--inkscape]

Options:
    -h --help   Show this screen.
    --version   Show version.
    -o --output Specify output file path. If not given, inferred
                from input by changing the file extension.
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

stom::pdf2svg(a$INPUT, a$output, page=stom::as_vec(a$page), white=a$white, backend=backend)
