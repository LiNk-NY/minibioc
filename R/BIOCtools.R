local_type_area <- function(
    base_repo_dir = minibioc_base_dir(),
    version = BiocManager::version(),
    type = getOption("pkgType"),
    uri = FALSE
) {
    paste0(
        if (uri) "file://",
        file.path(
            base_repo_dir,
            "packages",
            version,
            if (identical(type, "source"))
                "bioc"
            else
                file.path("container-binaries", "bioconductor_docker")
        )
    )
}

create_local_type_area <- function(
    base_repo_dir = minibioc_base_dir(),
    version = BiocManager::version(),
    type = getOption("pkgType"),
    include.Meta = TRUE,
    dry.run = TRUE
) {
    version_repo_dir <- local_type_area(
        base_repo_dir = base_repo_dir,
        version = version,
        type = type,
        uri = FALSE
    )
    repo_dir <-  file.path(
        utils::contrib.url(version_repo_dir),
        if (include.Meta && identical(type, "source")) "Meta" else ""
    )
    if (!dry.run & !dir.exists(repo_dir))
        dir.create(repo_dir, recursive = TRUE)
    repo_dir
}

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
        create_local_type_area(
            base_repo_dir = base_repo_dir,
            version = version,
            type = "source",
            include.Meta = TRUE,
            dry.run = TRUE
        ),
        rds_file
    )
}
