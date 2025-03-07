#' Create a minibioc local repository
#'
#' @examples
#' ## Point to source package directories
#' source_base_dir <- "~/bioc"
#' bioc_sub_pkgs <- file.path(
#'     source_base_dir, c(
#'         "SummarizedExperiment", "Biobase", "BiocBaseUtils",
#'         "BiocGenerics", "DelayedArray", "GenomicRanges",
#'         "IRanges", "S4Vectors"
#'     )
#' )
#'
#' ## minibioc source
#' repo_src_path <- create_mini_repo(
#'     src_pkg_dirs = bioc_sub_pkgs,
#'     base_repo_dir = repo_dir,
#'     type = "source"
#' )
#' ## add repository to repos option
#' options(repos = c(getOption("repos"), biocSrc = repo_src_path))
#'
#'
#' ## minibioc binaries
#' repo_bin_path <- create_mini_repo(
#'    src_pkg_dirs = bioc_sub_pkgs,
#'    base_repo_dir = repo_dir,
#'    type = "binary"
#' )
#'
#' @export
create_mini_repo <- function(
    src_pkg_dirs,
    base_repo_dir = minibioc_base_dir(),
    type = getOption("pkgType"),
    version = BiocManager::version()
) {
    if (!all(dir.exists(src_pkg_dirs)))
        stop("All source packages must be available locally")

    contrib_repo <- create_local_type_area(
        base_repo_dir = base_repo_dir,
        version = version,
        type = type,
        include.Meta = FALSE,
        dry.run = FALSE
    )

    build_fun <- switch(
        type,
        source = source_build,
        binary = binary_build
    )

    lapply(src_pkg_dirs, build_fun, dir = contrib_repo)

    tools::write_PACKAGES(
        dir = contrib_repo, addFiles = identical(type, "binary")
    )

    local_type_area(
        base_repo_dir = base_repo_dir,
        version = version,
        type = type,
        uri = TRUE
    )
}

binary_build <- function(pkg, dir) {
    old <- setwd(dir)
    on.exit(setwd(old))
    pkg <- source_build(pkg = pkg, dir = dir)
    install.packages(
        pkg,
        repos = NULL,
        type = "source",
        INSTALL_opts = "--build",
        update = FALSE,
        quiet = TRUE,
        ## whether to keep the .out files
        keep_outputs = FALSE,
        force = TRUE
    )
}

source_build <- function(pkg, dir) {
    devtools::build(pkg, path = dir, vignettes = FALSE)
}
