# Call as
#   Rscript  this.R  input.md
if (!interactive())
    args = commandArgs(TRUE)

# cat("cwd:", getwd(), "\n")
fin = args[1]
setwd(dirname(fin))
fin = basename(fin)

print("Compile to HTML")
fout = stom::replace_file_ext(fin, ".html")
stom::pandoc_html(fin, fout, args[-1])
