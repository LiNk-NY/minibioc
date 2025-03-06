
#' @examples
#'
#' library(BiocPkgTools)
#' bioc_sub <- pkgBiocDeps(
#'     "SummarizedExperiment", pkgType = "software",
#'     recursive = TRUE, only.bioc = TRUE
#' )
#' bioc_sub <- unlist(bioc_sub, use.names = FALSE)
#'
#' ## generate from Bioc package source dirs
#' packages <- file.path(normalizePath("~/bioc"), bioc_sub)
#' src_base <- "~/minibioc/packages/3.20/bioc"
#'
#' for (package in packages) {
#'    build_db_from_source(package, src_base)
#' }
#'
#' @export
build_db_from_source <- function(package_dir, src_base) {
    tmp_dir <- tempdir()
    package <- basename(package_dir)
    package_web_dir <- file.path(src_base, "web", "packages", package)
    if (!dir.exists(package_web_dir))
        dir.create(package_web_dir, recursive = TRUE)
    db <- tools::Rd_db(dir = package_dir)

    ## aliases.rds
    aliases <- lapply(db, tools:::.Rd_get_metadata, "alias")
    afile <- file.path(tmp_dir, "aliases.rds")
    saveRDS(aliases, file = afile, version = 2)
    atofile <- file.path(package_web_dir, "aliases.rds")
    file.copy(
        from = afile,
        to = atofile
    )
    message(atofile)

    ## rdxrefs.rds
    rdxrefs <- lapply(db, tools:::.Rd_get_xrefs)
    rdxrefs <- cbind(do.call(rbind, rdxrefs),
                     Source = rep.int(names(rdxrefs), sapply(rdxrefs, NROW)))
    xfile <- file.path(tmp_dir, "rdxrefs.rds")
    saveRDS(rdxrefs, file = xfile, version = 2)
    xtofile <- file.path(package_web_dir, "rdxrefs.rds")
    file.copy(
        from = xfile,
        to = xtofile
    )
    message(xtofile)
}

#' @examples
#' src_base <- "~/minibioc/packages/3.20/bioc/"
#' packages_dir <- file.path(src_base, "web", "packages")
#'
#' meta_folder <- file.path(contrib.url(src_base), "Meta")
#' if (!dir.exists(meta_folder)) dir.create(meta_folder, recursive = TRUE)
#' aliases_db_file <- file.path(meta_folder, "aliases.rds")
#'
#' meta_aliases_db <- build_meta_aliases_db(packages_dir, aliases_db_file)
#'
#' saveRDS(meta_aliases_db, aliases_db_file, version = 2)
#'
#' @export
build_meta_aliases_db <- function(packages_dir, aliases_db_file, force = FALSE) {
    files <- Sys.glob(file.path(packages_dir, "*", "aliases.rds"))
    packages <- basename(dirname(files))
    if(force || !is_file(aliases_db_file)) {
        db <- lapply(files, readRDS)
        names(db) <- packages
    } else {
        db <- readRDS(aliases_db_file)
        ## Drop entries in db not in package web area.
        db <- db[!is.na(match(names(db), packages))]
        ## Update entries for which aliases file is more recent than the
        ## db file.
        mtimes <- file.mtime(files)
        files <- files[mtimes >= file.mtime(aliases_db_file)]
        db[basename(dirname(files))] <- lapply(files, readRDS)
    }

    db[sort(names(db))]
}

is_file <- function(x) file.exists(x) && !file.info(x)[["isdir"]]

#' @examples
#' src_base <- "~/minibioc/packages/3.20/bioc/"
#' packages_dir <- file.path(src_base, "web", "packages")
#'
#' meta_folder <- file.path(contrib.url(src_base), "Meta")
#' if (!dir.exists(meta_folder)) dir.create(meta_folder, recursive = TRUE)
#' rdxrefs_db_file <- file.path(meta_folder, "rdxrefs.rds")
#'
#' meta_rdxrefs_db <- build_meta_rdxrefs_db(packages_dir, rdxrefs_db_file)
#'
#' saveRDS(meta_rdxrefs_db, rdxrefs_db_file, version = 2)
#'
#' @export
build_meta_rdxrefs_db <- function(packages_dir, rdxrefs_db_file, force = FALSE) {
    files <- Sys.glob(file.path(packages_dir, "*", "rdxrefs.rds"))
    packages <- basename(dirname(files))
    if(force || !is_file(rdxrefs_db_file)) {
        db <- lapply(files, readRDS)
        names(db) <- packages
    } else {
        db <- readRDS(rdxrefs_db_file)
        ## Drop entries in db not in package web area.
        db <- db[!is.na(match(names(db), packages))]
        ## Update entries for which rdxrefs file is more recent than the
        ## db file.
        mtimes <- file.mtime(files)
        files <- files[mtimes >= file.mtime(rdxrefs_db_file)]
        db[basename(dirname(files))] <- lapply(files, readRDS)
    }

    db[sort(names(db))]
}
