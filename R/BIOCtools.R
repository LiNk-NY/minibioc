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
