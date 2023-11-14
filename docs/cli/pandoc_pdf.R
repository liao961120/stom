# Call as
#   Rscript  this.R  input.md  [[--latex]]
if (!interactive())
    args = commandArgs(TRUE)

# cat("cwd:", getwd(), "\n")
fin = args[1]
tolatex = stom::cmd_has(args, "--latex")
setwd(dirname(fin))
fin = basename(fin)

if (tolatex) {
    print("Compile to LaTex")
    fout = stom::replace_file_ext(fin, ".tex")
    stom::pandoc_tex(fin, fout, style = "amsart", args[-1])
} else {
    fout = stom::replace_file_ext(fin, ".pdf")
    stom::pandoc_pdf(fin, fout, style = "amsart", args[-1])
}
