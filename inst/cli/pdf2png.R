VERSION = "0.0.1"

"Convert PDF to PNG(s) with inkscape.

Example:
    pdf2png input.pdf --page=1,3,5

Usage:
    pdf2png INPUT
    pdf2png INPUT [--dpi=DPI]
    pdf2png INPUT [--white]
    pdf2png INPUT [--page=PAGE]
    pdf2png INPUT [-o OUTFILE]
    pdf2png INPUT [--output=OUTFILE]

Options:
    -h --help   Show this screen.
    --version   Show version.
    -o --output Specify output file path. If not given, inferred
                from input by changing the file extension.
    --dpi=DPI   Dots per inch for output PNGs [default: 300].
    --white     Add white background.
    --page=PAGE The pages to convert.
" -> DOC
if (!interactive()) {
    a = docopt::docopt(DOC, version = VERSION)
} else {
    a = docopt::docopt(DOC, args=c("xxx.pdf"), version = VERSION)
}
# print(a)  # View parsed command line arguments

stom::pdf2png(a$INPUT, a$output, page=stom::as_vec(a$page), white=a$white, dpi=a$dpi)
