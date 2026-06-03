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
#'   it is the value of `local_bin_log()`.
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
#'     type = "source",
#'     logs_path = local_src_log()
#' )
#' ## add repository to repos option
#' options(repos = c(getOption("repos"), biocSrc = repo_src_path))
#'
#'
#' ## minibioc binaries
#' repo_bin_path <- create_mini_repo(
#'     src_pkg_dirs = bioc_sub_pkgs,
#'     base_repo_dir = repo_dir,
#'     type = "binary",
#'     logs_path = local_bin_log()
#' )
#' ## add repository to repos option
#' options(repos = c(biocBin = repo_bin_path, getOption("repos")))
#'
#' minibioc_local(
#'     build = "_software",
#'     depth0 = TRUE,
#'     dry.run = FALSE,
#'     ultimate_pkg = "IRanges",
#'     exclude_pkgs = c("canceR", "ChemmineOB", "flowCore")
#' )
#'
#' @export
minibioc_local <- function(
    bioc_version = BiocManager::version(),
    base_repo_dir = minibioc_base_dir(),
    build = c("_software", "_update", "_timings"),
    depth0 = FALSE,
    dry.run = TRUE,
    ultimate_pkg = character(),
    exclude_pkgs = character()
) {
    build <- match.arg(build)
    stopifnot(
        isScalarCharacter(base_repo_dir),
        is.package_version(bioc_version) || isScalarCharacter(bioc_version)
    )
    artifacts <- .bin_artifact_paths(
        base_repo_dir = base_repo_dir, version = bioc_version
    )

    image_name <- "bioconductor_docker"
    repos <- .repos(bioc_version, image_name, "local")

    local_create_cran_bucket(
        image_name = image_name,
        version = bioc_version,
        bucket = base_repo_dir
    )

    deps <- pkg_dependencies(
        version = bioc_version,
        build = build,
        binary_repo = repos$binary,
        ultimate_pkg = ultimate_pkg,
        exclude = exclude_pkgs,
        cloud_id = "local"
    )
    keep_ultimate <-
        if (length(ultimate_pkg)) names(deps) == ultimate_pkg else TRUE
    if (depth0)
        deps <- deps[lengths(deps) == 0L | keep_ultimate]

    minibioc_install(
        lib_path = artifacts$lib_path,
        bin_path = artifacts$bin_path,
        log_path = artifacts$log_path,
        deps = deps,
        dry.run = dry.run,
        BPPARAM = NULL
    )

    local_sync_artifacts(
        artifacts = artifacts, repos = repos
    )

    artifacts$bin_path
}

#' @rdname minibioc
#'
#' @param version `character(1)` The version of the Bioconductor repository
#'
#' @param bin_path `character(1)` The directory path of the local binary
#'   repository
#'
#' @param dry.run `logical(1)` Whether to run a dry run (list files to delete
#'   instead of deleting).
#'
#' @export
minibioc_clean <- function(
    version = BiocManager::version(),
    bin_path = local_bin_repo(),
    dry.run = TRUE
) {
    old <- setwd(bin_path)
    on.exit(setwd(old))

    repos <- .worker_repositories(version)
    db <- available.packages(repos = repos)
    ## software package dependencies
    contrib_url <- contrib.url(repos[["BioCsoft"]])
    idx <- db[, "Repository"] == contrib_url
    software_pkgs <- rownames(db)[idx]

    tars <- list.files(bin_path, pattern = "\\.tar\\.gz$")
    pkgs <- vapply(strsplit(tars, "_", fixed = TRUE), `[[`, character(1L), 1L)
    ext.pkgs <- tars[!pkgs %in% software_pkgs]

    if (dry.run) sort(ext.pkgs) else file.remove(ext.pkgs)
}
