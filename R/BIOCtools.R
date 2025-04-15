#' @export
read_CRAN_rds <- function(
    rds_db = c("aliases.rds", "rdxrefs.rds"),
    base_repo_dir = minibioc_base_dir(),
    version = BiocManager::version()
) {
    rds_db <- match.arg(rds_db)
    stopifnot(
        isScalarCharacter(rds_db),
        isScalarCharacter(base_repo_dir),
        is.package_version(version) || isScalarCharacter(version)
    )
    rds_db_file <- .rds_file_path(rds_file, base_repo_dir, version)
    if (!file.exists(rds_db_file))
        stop(
            rds_file, " file not found in ", version_repo_dir
        )
    tools:::read_CRAN_object(
        local_type_area(
            base_repo_dir = base_repo_dir,
            version = version,
            type = "source",
            uri = TRUE
        ),
        file.path(
            "src/contrib/Meta", rds_file
        )
    )
}

.rds_file_path <- function(rds_file, base_repo_dir, version) {
    file.path(
        local_src_repo(
            base_repo_dir = base_repo_dir,
            version = version,
            include.Meta = TRUE
        ),
        rds_file
    )
}

#' @keywords internal
.repos <-
    function(version, image_name, cloud_id = c('local', 'gcp', 'azure'))
{
    cloud <- match.arg(cloud_id)

    if (identical(cloud, "local")) {
        bucket <- file.path(minibioc_base_dir(), "packages/")
    }

    if (identical(cloud, "gcp")) {
        bucket <- paste0("gs://", "bioconductor-packages/")
    }

    if (identical(cloud, "azure")) {
        bucket <- "https://bioconductordocker.blob.core.windows.net/"
    }

    ## 'binary_repo' is where the existing binaries are located.
    ## 'cran_bucket' is where packages are uploaded on a google bucket
    binary_repo <- paste0(
        bucket, version, "/container-binaries/", image_name
    )
    cran_repo <- paste0(binary_repo, "/src/contrib/")
    logs_repo <- paste0(binary_repo, "/src/package_logs/")

    list(cran = cran_repo, binary = binary_repo, logs = logs_repo)
}
