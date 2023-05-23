# Call as:
#   Rscript this.R  input.R
if (!interactive())
    args = commandArgs(TRUE)

# Change directory to input file location
fin = args[1]
wd = dirname(fin)
fin = basename(fin)
setwd(wd)
fout = stom::replace_file_ext(fin, ".pdf")

stom::export_docs_pdf( fin, fout )
