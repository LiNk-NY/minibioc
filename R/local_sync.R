#' @name local_sync
#'
#' @title Create a CRAN style local repository
#'
#' @param image_name `character(1)` The name of the image used to build
#'   the binaries (default: "bioconductor_docker").
#'
#' @param bucket `character(1)` The folder that will contain this repository.
#'   It defaults to the output of `minibioc_base_dir()`.
#'
#' @importFrom BiocBaseUtils isScalarCharacter
#'
#' @return `local_create_cran_bucket` returns a character vector of the path to
#'     the binary repository.
#'
#' @examples
#' ## with minibioc_base_dir()
#' local_create_cran_bucket(
#'     image_name = "bioconductor_docker",
#'     version = "3.21",
#'     bucket = minibioc_base_dir()
#' )
#' ## on k8s
#' local_create_cran_bucket(
#'     image_name = "bioconductor_docker",
#'     version = "3.21",
#'     bucket = "/host/"
#' )
#' @export
local_create_cran_bucket <- function(
    image_name = "bioconductor_docker",
    version = BiocManager::version(),
    bucket = minibioc_base_dir()
) {
    stopifnot(isScalarCharacter(image_name))

    if (missing(bucket))
        bucket <- minibioc_base_dir()

    if (!length(bucket))
        stop(
            "Indicate a 'bucket' argument or set the",
            " 'BIOCONDUCTOR_BINARY_REPOSITORY' environment variable."
        )

    bucket <- file.path(bucket, "packages")

    bin_base <- file.path(
        bucket, version, "container-binaries", image_name
    )
    contrib_repo <- utils::contrib.url(bin_base)
    if (!dir.exists(contrib_repo))
        dir.create(contrib_repo, recursive = TRUE)

    contrib_repo
}

.file_move <- function(source, dest, pattern) {
    files <- list.files(source, pattern = pattern, full.names = TRUE)
    destfiles <- file.path(dest, basename(files))
    if (!dir.exists(dest))
        dir.create(dest)
    if (length(files))
        file.rename(files, destfiles)
}

.output_file_move <-
    function(artifacts)
{
    src <- list.files(artifacts$bin_path, full.names = TRUE, pattern = ".out$")
    dest <- paste0(artifacts$log_path, "/", basename(src))

    if (length(src))
        file.rename(src, dest)
}

#' Sync build artifacts and logs to the local storage
#'
#' @param artifacts `list` A list of paths for build artifacts as returned by
#'   `.bin_artifact_paths()`.
#'
#' @param repos `list` of repository paths as returned by internal `.repos()`.
#'
#' @export
local_sync_artifacts <-  function(artifacts, repos) {
    log_file <- file.path(artifacts$log_path, 'minibioc_install.log')
    flog.appender(appender.tee(log_file), name = 'minibioc_install')

    ## Move .out files from bin_path to logs_path
    ## This avoids duplicate copy of PACKAGES* files to contrib(cran path)
    ## and to package_logs
    .output_file_move(artifacts)
    flog.info(
        'Moved .out files to %s: ', artifacts$log_path,
        name = 'minibioc_install'
    )

    ## Sync logs from /host/logs_3_13 to /src/package_logs
    .file_move(artifacts$log_path, repos$logs, "\\.log$")
    flog.info(
        'Finished moving logs to local storage: %s',
        artifacts$log_path,
        name = 'minibioc_install'
    )
}
