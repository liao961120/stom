# Call as:
#   Rscript this.R  input.pdf   [--to=png/svg]  [--dpi=900]
out_ext = ".svg"
if (!interactive())
    args = commandArgs(TRUE)
if ( stom::cmd_has(args, "--to=png") )
    out_ext = ".png"
dpi = stom::cmd_get_value(args, "--dpi=")
if ( !is.na(dpi) ) {
    dpi = as.integer(dpi)
} else {
    dpi = 900
}

# Change directory to input file location
fin = args[1]
wd = dirname(fin)
fin = basename(fin)
setwd(wd)
fout = stom::replace_file_ext(fin, out_ext)


# print(dpi)
# stop()
if (out_ext == ".png") {
    stom::pdf2png(fin, fout, dpi=dpi)
} else {
    stom::pdf2svg(fin, fout)
}
