# GDrive for model cache

#' Gather Stan models for Google Drive upload
#' @seealso [save_model()]
#' @export
prepare_drive_model = function() {
    tgt = stom_root("ToGDrive")
    if (dir.exists(tgt)) unlink(tgt, recursive = T)
    dir.create(tgt)
    src = list_local_models()
    for ( s in src ) {
        from = stom_root(s)
        to = file.path( tgt, s )
        to_dir = dirname(to)
        if ( !dir.exists(to_dir) )
            dir.create(to_dir, recursive = T)
        file.copy(from, to, overwrite = T)
    }
    info = lapply( src, function(s) {
        x = file.info(stom_root(s))
        list( size = paste( round(x$size/(1024^2),2), "MB" ),
              mtime = x$mtime,
              ctime = x$ctime)
    })
    names(info) = src
    info = jsonlite::toJSON( info, pretty = T, auto_unbox = T )
    writeLines( info, file.path(tgt, "info.json") )
    url = "https://drive.google.com/drive/folders/1qN5ppDPBiLc5is6JXM_M8OJJ1J5W_4vy"
    cli::cli_alert_success("RDS models copied to {.file {tgt}}")
    cli::cli_alert_success("Upload them to {.href [Stom's Google Drive]({url})}")
}


list_local_models = function() {
    fps = list.files("inst", "\\.RDS$", recursive = T, full.names = T)
    fps
}


stom_root = function(files=NULL) {
    root = rstudioapi::getActiveProject()
    if (is.null(root))
        root = rprojroot::find_root( rprojroot::is_git_root )
    if (!is.null(files))
        root = file.path( root, files )
    root
}
