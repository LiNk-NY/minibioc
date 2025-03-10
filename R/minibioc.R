#' Create a minibioc local repository
#'
#' @param src_pkg_dirs `character()` A vector of package names or their local
#'   source directories (when buliding tarballs).
#'
#' @param base_repo_dir `character(1)` The base directory where the CRAN-like
#'   repository will be created.
#'
#' @param type `character(1)` The type of repository to create. Either "source"
#'   or "binary". By default, it the value of `getOption("pkgType")`.
#'
#' @param version `character(1)` The version of the repository to create. By
#'   default, it is the value of `BiocManager::version()`.
#'
#' @param logs_path `character(1)` The path to the logs directory. By default,
#'   it is the value of `getOption("minibioc.logs")`.
#'
#' @importFrom BiocBaseUtils isCharacter
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
#' library(ReleaseLaunch)
#' ## gitcreds::gitcreds_set()
#' update_local_repos(repos_dir = source_base_dir, org = "Bioconductor")
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
#'     type = "source",
#'     logs_path = "~/data/logs"
#' )
#'
#' @export
create_mini_repo <- function(
    src_pkg_dirs,
    base_repo_dir = minibioc_base_dir(),
    type = getOption("pkgType"),
    version = BiocManager::version(),
    logs_path = getOption("minibioc.logs")
) {
    if (!all(dir.exists(src_pkg_dirs)))
        stop("All source packages must be available locally")

    stopifnot(
        isCharacter(src_pkg_dirs),
        isScalarCharacter(base_repo_dir),
        isScalarCharacter(type),
        is.package_version(version) || isScalarCharacter(version),
        isScalarCharacter(logs_path) && dir.exists(logs_path)
    )

    if (!dir.exists(logs_path))
        dir.create(logs_path, recursive = TRUE)

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
        logs_path = logs_path
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
