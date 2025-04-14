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
