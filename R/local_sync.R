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
    if (length(files))
        file.rename(files, destfiles)
}

#' @export
local_sync_artifacts <-  function(artifacts, repos) {
    log_file <- file.path(artifacts$log_path, 'minibioc_install.log')
    flog.appender(appender.tee(log_file), name = 'minibioc_install')

    ## Move .out files from bin_path to logs_path
    ## This avoids duplicate copy of PACKAGES* files to contrib(cran path)
    ## and to package_logs
    .output_file_move(artifacts)
    flog.info(
        'Moved .out files to %s: ', artifacts$logs_path,
        name = 'minibioc_install'
    )

    ## Sync binaries from /host/binary_3_13 to /src/contrib/
    .file_move(artifacts$bin_path, repos$binary, "\\.tar\\.gz$")
    flog.info(
        'Finished moving binaries to local storage: %s',
        artifacts$bin_path,
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
