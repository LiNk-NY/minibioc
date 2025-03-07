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

read_aliases_db <- function(
    base_repo_dir = minibioc_base_dir(),
    version = BiocManager::version()
) {
    version_repo_dir <- create_local_type_area(
        base_repo_dir = base_repo_dir,
        version = version,
        type = "source",
        include.Meta = TRUE,
        dry.run = TRUE
    )
    aliases_db_file <- file.path(version_repo_dir, "aliases.rds")
    if (!file.exists(aliases_db_file))
        stop(
            "aliases.rds file not found in ", version_repo_dir
        )
    tools:::read_CRAN_object(
        local_type_area(
            base_repo_dir = base_repo_dir,
            version = version,
            type = "source",
            uri = TRUE
        ),
        "src/contrib/Meta/aliases.rds"
    )
}

read_rdxrefs_db <- function(
    base_repo_dir = minibioc_base_dir(),
    version = BiocManager::version()
) {
    version_repo_dir <- create_local_type_area(
        base_repo_dir = base_repo_dir,
        version = version,
        type = "source",
        include.Meta = TRUE,
        dry.run = TRUE
    )
    rdxrefs_file <- file.path(version_repo_dir, "rdxrefs.rds")
    if (!file.exists(rdxrefs_file))
        stop(
            "rdxrefs.rds file not found in ", version_repo_dir
        )
    tools:::read_CRAN_object(
        local_type_area(
            base_repo_dir = base_repo_dir,
            version = version,
            type = "source",
            uri = TRUE
        ),
        "src/contrib/Meta/rdxrefs.rds"
    )
}
