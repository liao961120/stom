# Call as
#   Rscript  this.R  input.md
if (!interactive())
    args = commandArgs(TRUE)

# cat("cwd:", getwd(), "\n")
fin = args[1]
setwd(dirname(fin))
fin = basename(fin)
fout = stom::replace_file_ext(fin, ".pdf")
stom::pandoc_pdf(fin, fout, style = "amsart")

