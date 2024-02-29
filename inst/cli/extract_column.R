"Extract a column from a tabular file.

Examples:
    extract_column iris.csv Species -o out.txt
    extract_column iris.xlsx Species -o out.txt
    extract_column iris.xlsx --sheet=1 Species -o out.txt
    extract_column iris.xlsx Species -o out.txt -s 1

Usage:
    extract_column INPUT [ --sheet=<s>] COL_NAME ( -o OUTFILE | --output=OUTFILE )

Options:
    -h --help           Show this screen.
    --output=<o> -o <o> Specify output file path. If not given, inferred
                           from input by changing the file extension.
    --sheet=<s> -s <s>  Sheet index (for EXCEL inputs only).
" -> DOC
if (!interactive()) {
    a = docopt::docopt(DOC)
} else {
    a = docopt::docopt(DOC, args=c(
        "iris.xlsx",
        "Species",
        "-o", "out.txt",
        #"-s", "1",
        NULL
        ))
}
# print(a)
#############

if (tools::file_ext(a$INPUT) %in% c("xlsx", "xls", "xlsm") )
    d = readxl::read_excel(a$INPUT, col_types = "text")
if (tools::file_ext(a$INPUT) == "csv")
    d = read.csv(a$INPUT, colClasses = "character")

message("File written to ", a$output)
xfun::write_utf8(d[[a$COL_NAME]], a$output)

