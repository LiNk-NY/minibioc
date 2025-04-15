local_type_area <- function(
    base_repo_dir = minibioc_base_dir(),
    version = BiocManager::version(),
    type = getOption("pkgType"),
    uri = FALSE,
    image_name = "bioconductor_docker"
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
                file.path("container-binaries", image_name)
        )
    )
}

#' @export
local_bin_repo <- function(
    base_repo_dir = minibioc_base_dir(),
    version = BiocManager::version(),
    contrib.url = TRUE,
    uri = FALSE,
    ...
) {
    version_repo_dir <- local_type_area(
        base_repo_dir = base_repo_dir,
        version = version,
        type = "binary",
        uri = uri,
        ...
    )
    if (contrib.url)
        version_repo_dir <- utils::contrib.url(version_repo_dir)
    version_repo_dir
}

#' @export
local_src_repo <- function(
    base_repo_dir = minibioc_base_dir(),
    version = BiocManager::version(),
    contrib.url = TRUE,
    include.Meta = FALSE,
    uri = FALSE,
    ...
) {
    version_repo_dir <- local_type_area(
        base_repo_dir = base_repo_dir,
        version = version,
        type = "source",
        uri = uri,
        ...
    )
    if (contrib.url)
        version_repo_dir <- utils::contrib.url(version_repo_dir)
    repo_dir <-  file.path(
        version_repo_dir,
        if (include.Meta) "Meta" else ""
    )
    repo_dir
}

#' @export
local_src_log <- function(
    base_dir = minibioc_base_dir(),
    version = BiocManager::version()
) {
    ver <- gsub(".", "_", version, fixed = TRUE)

    artifact_path <-
        file.path(base_dir, paste("source", ver, "logs", sep = "_"))

    if (!dir.exists(artifact_path)) {
        dir.create(artifact_path, recursive = TRUE)
        flog.info(
            "created path: %s", artifact_path,
            name = "minibioc_install"
        )
    }

    artifact_path
}

#' @export
local_bin_log <- function(
    base_dir = minibioc_base_dir(),
    version = BiocManager::version()
) {
    ver <- gsub(".", "_", version, fixed = TRUE)

    artifact_path <-
        file.path(base_dir, paste("binary", ver, "logs", sep = "_"))

    if (!dir.exists(artifact_path)) {
        dir.create(artifact_path, recursive = TRUE)
        flog.info(
            "created path: %s", artifact_path,
            name = "minibioc_install"
        )
    }

    artifact_path
}

