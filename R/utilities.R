#' @name local-file-utils
#'
#' @title Helper functions to manage local repository paths and directories
#'
#' @description These functions are used to manage the local repository paths
#'   and directories for both source and binary packages. They provide a
#'   consistent way to construct paths to the local repositories, logs, and
#'   library directories based on the base repository directory and Bioconductor
#'   version. The `local_type_area` function is a helper function that
#'   constructs the path to either the source or binary repository based on the
#'   specified type. The `local_bin_repo` and `local_src_repo` functions use
#'   `local_type_area` to get the paths to the binary and source repositories,
#'   respectively. The `local_src_log`, `local_bin_log`, and `local_library`
#'   functions create and return paths for logs and library directories,
#'   ensuring that they exist before returning the path.
#'
#' @details \preformatted{
#' - local_type_area:   Helper function to construct path based on Bioc version,
#'                      type (source or binary), image_name (for binaries), etc.
#' - local_bin_repo:    Constructs path to the local binary repository.
#' - local_src_repo:    Constructs path to the local source repository.
#' - local_src_log:     Constructs path to the local source build logs.
#' - local_bin_log:     Constructs path to the local binary build logs.
#' - local_library:     Constructs path to the local library directory.
#' }
#'
#' @examplesIf interactive()
#'
#' local_src_repo() |>
#'     dir.create(recursive = TRUE)
#'
#' local_bin_repo() |>
#'     dir.create(recursive = TRUE)
#'
#' @export
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

#' @rdname local-file-utils
#'
#' @param base_repo_dir `character(1)` The base directory of the repository.
#'
#' @param version `character(1)` The Bioconductor version of the repository.
#'
#' @param contrib.url `logical(1)` Whether to append the standard R contrib
#'   path.
#'
#' @param uri `logical(1)` Whether to prepend 'file://'.
#'
#' @param ... Additional arguments passed to `local_type_area`.
#'
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

#' @rdname local-file-utils
#'
#' @param base_repo_dir `character(1)` The base directory of the repository.
#'
#' @param version `character(1)` The version of the repository.
#'
#' @param contrib.url `logical(1)` Whether to append the standard R contrib
#'   path.
#'
#' @param include.Meta `logical(1)` Whether to include the Meta directory in the
#'   path.
#'
#' @param uri `logical(1)` Whether to prepend 'file://'.
#'
#' @param ... Additional arguments passed to `local_type_area`.
#'
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

#' @rdname local-file-utils
#'
#' @param base_dir `character(1)` The base repository directory.
#'
#' @param version `character(1)` The version of the repository.
#'
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

#' @rdname local-file-utils
#'
#' @param base_dir `character(1)` The base repository directory.
#'
#' @param version `character(1)` The version of the repository.
#'
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

#' @rdname local-file-utils
#'
#' @param base_repo_dir `character(1)` The base directory of the repository.
#'
#' @param version `character(1)` The version of the repository.
#'
#' @export
local_library <- function(
        base_repo_dir = minibioc_base_dir(),
        version = BiocManager::version()
) {
    ver <- gsub(".", "_", version, fixed = TRUE)
    artifact_path <-
        file.path(base_repo_dir, "lib", "R", paste("bioc", ver, sep = "_"))

    if (!dir.exists(artifact_path)) {
        dir.create(artifact_path, recursive = TRUE)
        flog.info(
            "created path: %s", artifact_path,
            name = "minibioc_install"
        )
    }

    artifact_path
}
