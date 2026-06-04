#' @rdname minibioc
#'
#' @title Create a minibioc local repository
#'
#' @param bioc_version `character(1)` The version of the repository/Bioconductor
#'   to create. By default, it is the value of `BiocManager::version()`.
#'
#' @param base_repo_dir `character(1)` The base directory where the CRAN-like
#'   repository will be created. By default, it is the value of
#'   `minibioc_base_dir()`.
#'
#' @param build `character(1)` The build mode, one of `"_software"`,
#'   `"_update"`, or `"_timings"`.
#'
#' @param depth0 `logical(1)` Whether to only install/build packages with zero
#'   dependencies (plus the `ultimate_pkg` if specified). By default, `FALSE`.
#'
#' @param dry.run `logical(1)` Whether to run a dry run (not actually installing
#'   the packages). By default, `TRUE`.
#'
#' @param ultimate_pkg `character()` The last package in the queue to start
#'   from especially useful after interrupted builds.
#'
#' @param exclude_pkgs `character()` Packages to exclude from the dependency
#'   list.
#'
#' @importFrom BiocBaseUtils isScalarCharacter
#'
#' @examplesIf interactive()
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
#' ## Set up git credentials for private repositories if needed
#' gitcreds::gitcreds_set()
#'
#' ## Git sync local source repositories
#' ReleaseLaunch::update_local_repos(
#'     repos_dir = source_base_dir, org = "Bioconductor"
#' )
#'
#' ## specify minibioc base repository directory
#' repo_dir <- minibioc_base_dir()
#'
#' ## Build local source package tarballs in the local source repository
#' for (pkg in bioc_sub_pkgs) {
#'     build_source_package(
#'         pkg = pkg,
#'         lib_path = local_library(),
#'         bin_path = local_src_repo(),
#'         log_path = local_src_log()
#'     )
#' }
#'
#' ## Build local binaries and manage the local CRAN-like repository
#' minibioc_local(
#'     base_repo_dir = repo_dir,
#'     build = "_software",
#'     depth0 = TRUE,
#'     dry.run = TRUE,
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
