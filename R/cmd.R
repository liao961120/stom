##############################################
##### Functions for command line program #####
##############################################

#' Get stom cmd programs
#' @export
#' @examples
#' get_cli_programs()
get_cli_programs = function(git_bash_path=T) {
    dir_ = system.file("cli", package="stom")
    x = list.files(dir_, full.names = T, include.dirs = F)
    x = x[endsWith(x, ".R")]
    if (git_bash_path)
        return( to_gitbash_path(x) )
    x
}


#' Full parsing of command line arguments
#'
#' @param x A vector of command line arguments.
#' @return A list of parsed arguments
#' @examples
#' x = c("--date=2021", "--hi-w=y", "--hi_w=y", "./file-info.txt", "--hi")
#' cmd_parse_args(x)
#' @export
cmd_parse_args = function(x) {
    list(
        args  = x[is_normal_arg(x)],
        named = cmd_parse_key_val(x[is_key_val(x)]),
        opts  = x[is_option(x)]
    )
}

#' @rdname cmd_parse_args
cmd_parse_key_val = function(x) {
    x = trimws(x, "left", whitespace = "--")
    x = strsplit(x, "=", fixed=T)
    key = sapply(x, function(e) e[[1]])
    val = sapply(x, function(e) e[[2]])
    names(val) = key
    val
}

is_key_val = function(args) {
    pat = "^--[a-zA-Z0-9]+[a-zA-Z0-9_-]*="
    x = grepl( pat, args )
    names(x) = args
    x
}
is_option = function(args) {
    pat = "^--[a-zA-Z0-9]+[a-zA-Z0-9_-]*$"
    x = grepl( pat, args )
    names(x) = args
    x
}
is_normal_arg = function(args) {
    x = !( is_key_val(args) | is_option(args) )
    names(x) = args
    x
}



#' Primitive parsing of command line arguments
#'
#' @param args Character vector. Returned by `base::commandArgs(TRUE)`.
#' @param pre String. Command line prefix to retrieve value from.
#' @param has Character vector. Arguments to be checked for presence in the
#'        command line arguments.
#'
#' @examples
#' args = c( "--date=2022-11-20", "--second" )
#' cmd_get_value(args, "--date=")
#' cmd_parse_date(args)
#' cmd_has(args, "--second")
#' @export
cmd_get_value = function(args, pre) {
    idx = which( startsWith(args, prefix=pre) )
    val = substring( args[idx], nchar(pre)+1 )
    if (length(val) == 0) return(NA)
    return(val)
}


#' @rdname cmd_get_value
#' @export
cmd_has = function(args, has)
    has %in% args


#' @rdname cmd_get_value
#' @export
cmd_parse_date = function(args, pre="--date=") {
    cmd_get_value(args, pre)
}




