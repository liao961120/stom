VERSION = "0.0.1"

"Convert Pandoc's Markdown to PDF.

Usage:
    pandoc_pdf <fin>
    pandoc_pdf <fin> [-o <outfp>]
    pandoc_pdf <fin> [--output=<outfp>]
    pandoc_pdf <fin> [--latex]
    pandoc_pdf <fin> [--pandoc -- <pd_args>...]

Options:
    -h --help   Show this screen.
    --version   Show version.
    -o --output Specify output file path. If not given, inferred
                from input by changing the file extension.
    --latex     Output `.tex` instead of `.pdf`.
    --pandoc    Specify additional arguments to pass to Pandoc.
" -> DOC
if (!interactive()) {
    a = docopt::docopt(DOC, version = VERSION)
} else {
    a = docopt::docopt(DOC, args=c("xxx.md"), version = VERSION)
}
# print(a)  # View parsed command line arguments

# Determine output path
fin = tools::file_path_as_absolute(a$fin)
fout = stom::replace_file_ext(fin, ifelse(a$latex,".tex",".pdf"))
fout = file.path(getwd(), basename(fout))  # Generate output to CWD by default
if (!is.null(a$output))
    fout = tools::file_path_as_absolute(a$output)

# Compile document
setwd(dirname(fin))  # WD as input file's directory
if (a$latex) {
    message("Compile to LaTex instead of PDF")
    stom::pandoc_tex(fin, fout, style = "amsart", unlist(a$pd_args))
} else {
    stom::pandoc_pdf(fin, fout, style = "amsart", unlist(a$pd_args))
}


# Print I/O info
cat("\n")
cat("[File I/O]\n")
stom::lst2message(list(
    INPUT  = fin,
    OUTPUT = fout
    ), indent=3)
