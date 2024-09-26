"Joining multiple SVG/PNG images into a single one.

Example:
    img_join A.svg B.svg C.svg -o output.png

Usage:
    img_join INPUT... -o FILE [--stack] [--height=H] [--width=W] [--sep_fct=S] [--background=bg]

Options:
    -h --help              Show this screen.
    -o FILE --output=FILE  Specify output file path.
    --stack                Join image vertically.
    --height=H             The height, in pixels, of the image to be converted to (SVG only).
    --width=W              The width, in pixels, of the image to be converted to (SVG only).
    --sep_fct=S            A value determining the space between the joined images [default: 0.1].
    --background=bg        Background color code [default: white].
" -> DOC
if (!interactive()) {
    a = docopt::docopt(DOC)
} else {
    a = docopt::docopt(DOC, args=stom::as_vec("hdcm-dag.svg hdcm-tree-en.svg --height=300 -o out.png") )
}
# print(a)  # View parsed command line arguments


w = a$width # ifelse( is.null(a$width), NULL,   as.numeric(a$width) )
h = a$height # ifelse( is.null(a$height), NULL,  as.numeric(a$height) )
s = as.numeric(a$sep_fct)
stom::image_combine(a$INPUT, output = a$output,
                    stack = a$stack, sep_fct = s, height = h, width =w,
                    background = a$background)
