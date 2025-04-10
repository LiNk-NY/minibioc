#' @name minibioc_single_package
#'
#' @title Install and create binaries for R packages
#'
#' @description Generalized from `BiocKubeInstall`. This function installs a
#'  single R package and creates its binary file for installation.
#'
#' @details The package given by `pkg` is installed in the given library path
#'   `lib_path`, and the binaries are created in the `dest_path`.
#'
#' @param pkg `character(1)` name of R or Bioconductor package.
#'
#' @param lib_path `character(1)` path where R package libraries are stored.
#'
#' @param dest_path `character(1)` path where R package binaries are stored.
#'
#' @param logs_path `character(1)` path where R package binary build logs are
#'   stored.
#'
#' @importFrom BiocManager install
#' @importFrom futile.logger flog.appender flog.info flog.error appender.tee
#'
#' @returns `install_binary_package()` returns invisibly
#'
#' @examples
#' repo_bin_path <- paste0(
#'     "/home/rstudio/minibioc/packages/3.21/",
#'     "container-binaries/bioconductor_docker/"
#' )
#' install_binary_package(
#'     pkg = "BiocParallel",
#'     lib_path = NULL,
#'     dest_path = utils::contrib.url(repo_bin_path),
#'     logs_path = "~/data/logs"
#' )
#' @export
install_binary_package <-
    function(pkg, lib_path, dest_path = minibioc_base_dir(), logs_path)
{
    .libPaths(c(lib_path, .libPaths()))

    pkg <- basename(pkg)
    log_file <- file.path(logs_path, 'minibioc_install.log')
    flog.appender(appender.tee(log_file), name = 'minibioc_install')

    flog.info("building binary for package: %s", pkg, name = 'minibioc_install')

    if (missing(dest_path))
        dest_path <- create_local_type_area(
            base_repo_dir = dest_path, type = "binary", dry.run = FALSE
        )
    cwd <- setwd(dest_path)
    on.exit(setwd(cwd))

    ## The default return value for a success package building
    result <- pkg

    withCallingHandlers({
        suppressMessages(
            BiocManager::install(
                pkg,
                type = "source",
                INSTALL_opts = "--build",
                update = FALSE,
                quiet = TRUE,
                force = TRUE,
                ## TODO: a successful install output isn't useful
                keep_outputs = TRUE
            )
        )
    },
    error = function(e) {
        flog.error("Error: package %s failed", pkg, name = "minibioc_install")
        result <<- e
    },
    warning = function(e) {
        flog.error("Error: package %s failed", pkg, name = "minibioc_install")
        result <<- e
        tryInvokeRestart("muffleWarning")
    }
    )
    result
}

#' @rdname minibioc_single_package
#'
#' @examples
#' repo_src_path <- "/home/rstudio/minibioc/packages/3.21/bioc/"
#' build_source_package(
#'     pkg = "~/bioc/BiocParallel",
#'     lib_path = NULL,
#'     dest_path = utils::contrib.url(repo_src_path),
#'     logs_path = "~/data/logs"
#' )
#' @export
build_source_package <-
    function(pkg, lib_path, dest_path, logs_path)
{
    .libPaths(c(lib_path, .libPaths()))

    log_file <- file.path(logs_path, 'minibioc_build.log')
    flog.appender(appender.tee(log_file), name = 'minibioc_build')

    flog.info(
        "building source package: %s", basename(pkg), name = 'minibioc_build'
    )
    cwd <- setwd(dest_path)
    on.exit(setwd(cwd))

    ## The default return value for a success package building
    result <- pkg

    withCallingHandlers({
        suppressMessages(
            pkgbuild::build(
                path = pkg,
                dest_path = dest_path
            )
        )
    },
    error = function(e) {
        flog.error("Error: package %s failed", pkg, name = "minibioc_build")
        result <<- e
    },
    warning = function(e) {
        flog.error("Error: package %s failed", pkg, name = "minibioc_build")
        result <<- e
        tryInvokeRestart("muffleWarning")
    }
    )
    result
}
