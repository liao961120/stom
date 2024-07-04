VERSION = "0.0.1"

"Convert PDF to SVG(s) with inkscape.

Example:
    pdf2svg input.pdf --page=1,3,5

Usage:
    pdf2svg INPUT
    pdf2svg INPUT [-o OUTFILE]
    pdf2svg INPUT [--output=OUTFILE]
    pdf2svg INPUT [--white]
    pdf2svg INPUT [--page=PAGE]

Options:
    -h --help   Show this screen.
    --version   Show version.
    -o --output Specify output file path. If not given, inferred
                from input by changing the file extension.
    --white     Add white background.
    --page=PAGE The pages to convert.

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

stom::pdf2svg(a$INPUT, a$output, page=stom::as_vec(a$page), white=a$white)
