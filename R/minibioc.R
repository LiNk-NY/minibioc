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
#' ## specify minibioc base repository directory
#' repo_dir <- minibioc_base_dir()
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
#' ## add repository to repos option
#' options(repos = c(biocBin = repo_bin_path, getOption("repos")))
#'
#' ## run in parallel
#' library(BiocParallel)
#'
#' bpparam <- MulticoreParam(workers = 12)
#' register(bpparam)
#'
#' bplapply(
#'     bioc_sub_pkgs,
#'     create_mini_repo,
#'     base_repo_dir = repo_dir,
#'     type = "source"
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
        source = minibioc_build_single_package,
        binary = minibioc_install_single_package
    )

    lapply(
        src_pkg_dirs,
        build_fun,
        dest_path = contrib_repo,
        lib_path = NULL,
        logs_path = "~/data/logs"
    )

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
