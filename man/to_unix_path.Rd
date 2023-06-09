% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sys.R
\name{to_unix_path}
\alias{to_unix_path}
\alias{to_gitbash_path}
\alias{print_paths}
\title{Normalize to Unix paths}
\usage{
to_unix_path(x)

to_gitbash_path(x)

print_paths(x, win = T)
}
\arguments{
\item{x}{Character. Paths.}
}
\value{
Character
}
\description{
Normalize to Unix paths
}
\examples{
fp = 'C:\\\\Users\\\\rd\\\\bin\\\\export_docs_pdf.R'
to_unix_path(fp)
to_gitbash_path(fp)
get_cli_programs(F) |> print_paths()
}
