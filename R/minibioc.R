#' Create a minibioc local repository
#'
#' @examples
#' ## Point to source package directories
#' setwd("~/bioc")
#'
#' bioc_sub <- c(
#'     "SummarizedExperiment", "Biobase", "BiocBaseUtils",
#'     "BiocGenerics", "DelayedArray", "GenomicRanges",
#'     "IRanges", "S4Vectors"
#' )
#'
#' ## minibioc source
#' src_base <- "~/minibioc/packages/3.20/bioc/"
#' if (!dir.exists(src_base))
#'     dir.create(src_base, recursive = TRUE)
#' repo_src_path <- paste0("file:///", normalizePath(src_base))
#' create_mini_repo(
#'     bioc_sub,
#'     dir = src_base,
#'     type = "source"
#' )
#' options(repos = c(getOption("repos"), biocSrc = repo_src_path))
#'
#'
#' ## minibioc binaries
#' bin_base <- "~/minibioc/packages/3.20/container-binaries/bioconductor_docker"
#' if (!dir.exists(bin_base))
#'    dir.create(bin_base, recursive = TRUE)
#' repo_bin_path <- paste0("file:///", normalizePath(bin_base))
#' create_mini_repo(
#'    bioc_sub,
#'    dir = bin_base,
#'    type = "binary"
#' )
#'
#' @export
create_mini_repo <- function(packages, dir, type = getOption("pkgType"))
{
    if (!all(dir.exists(packages)))
        stop("All source packages must be available locally")
    contrib_repo <- utils::contrib.url(dir)
    if (!dir.exists(contrib_repo))
        dir.create(contrib_repo, recursive = TRUE)

    build_fun <- switch(
        type,
        source = source_build,
        binary = binary_build
    )

    lapply(packages, build_fun, dir = contrib_repo)

    tools::write_PACKAGES(
        dir = contrib_repo, addFiles = identical(type, "binary")
    )
    dir
}

binary_build <- function(pkg, dir) {
    old <- setwd(dir)
    on.exit(setwd(old))
    BiocManager::install(
        pkg,
        INSTALL_opts = "--build",
        update = FALSE,
        quiet = TRUE,
        ## whether to keep the .out files
        keep_outputs = FALSE,
        force = TRUE
    )
}

source_build <- function(pkg, dir) {
    devtools::build(pkg, path = dir, vignettes = FALSE)
}
