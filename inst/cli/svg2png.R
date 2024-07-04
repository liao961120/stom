VERSION = "0.0.1"

"Convert SVG to PNG with inkscape.

Example:
    svg2png input.svg

Usage:
    svg2png INPUT
    svg2png INPUT [-o OUTFILE]
    svg2png INPUT [--dpi=DPI]
    svg2png INPUT [--output=OUTFILE]

Options:
    -h --help   Show this screen.
    --version   Show version.
    -o --output Specify output file path. If not given, inferred
                from input by changing the file extension.
    --dpi=DPI   Dots per inch for output PNGs [default: 350].
" -> DOC
if (!interactive()) {
    a = docopt::docopt(DOC, version = VERSION)
} else {
    a = docopt::docopt(DOC, args=c("xxx.svg"), version = VERSION)
}
# print(a)  # View parsed command line arguments

stom::svg2png(a$INPUT, a$output, dpi=as.integer(a$dpi))
