#' @name minibioc_single
#'
#' @title Install and create binaries for R packages
#'
#' @description Generalized from `BiocKubeInstall`. This function installs a
#'  single R package and creates its binary file for installation.
#'
#' @details The package given by `pkg` is installed in the given library path
#'   `lib_path`, and the binaries are created in the `bin_path`.
#'
#' @param pkg `character(1)` name of R or Bioconductor package.
#'
#' @param lib_path `character(1)` path where R package libraries are stored.
#'
#' @param bin_path `character(1)` path where R package binaries are stored.
#'
#' @param log_path `character(1)` path where R package binary build logs are
#'   stored.
#'
#' @importFrom BiocManager install
#' @importFrom futile.logger flog.appender flog.info flog.error appender.tee
#'
#' @returns `install_build_binary()` returns invisibly
#'
#' @examplesIf interactive()
#' ## Point to source package directories
#' source_base_dir <- "~/bioc"
#' bioc_sub_pkgs <- file.path(
#'     source_base_dir, c(
#'         "SummarizedExperiment", "Biobase", "BiocBaseUtils",
#'         "BiocGenerics", "DelayedArray", "GenomicRanges",
#'         "IRanges", "S4Vectors"
#'     )
#' )
#'
#' ## Install local binaries for a single package
#' for (pkg in bioc_sub_pkgs) {
#'     install_build_binary(
#'         pkg = pkg,
#'         dry.run = FALSE,
#'         lib_path = local_library(),
#'         bin_path = local_bin_repo(),
#'         log_path = local_bin_log()
#'     )
#' }
#'
#' ## Create REPOSITORY files for the local source and binary repositories
#' biocViews::write_REPOSITORY(
#'     local_src_repo(contrib.url = FALSE),
#'     contribPaths = c(
#'         "source" = "src/contrib"
#'     )
#' )
#'
#' biocViews::write_REPOSITORY(
#'     local_bin_repo(contrib.url = FALSE),
#'     contribPaths = c(
#'         "linux.binary" = "src/contrib"
#'     )
#' )
#'
#' ## Create PACKAGES, PACKAGES.gz, PACAKGES.rds for the local binary repository
#' tools::write_PACKAGES(local_bin_repo(), addFiles = TRUE, verbose = TRUE)
#'
#' ## Create PACKAGES, PACKAGES.gz, PACAKGES.rds for the local source repository
#' tools::write_PACKAGES(local_src_repo(), addFiles = TRUE, verbose = TRUE)
#'
#' @export
install_build_binary <-
    function(pkg, dry.run, lib_path, bin_path = local_bin_repo(), log_path)
{
    .libPaths(c(lib_path, .libPaths()))

    pkg <- basename(pkg)
    log_file <- file.path(log_path, 'minibioc_install.log')
    flog.appender(appender.tee(log_file), name = 'minibioc_install')

    flog.info(
        "building binary for package: %s", pkg, name = 'minibioc_install'
    )

    cwd <- setwd(bin_path)
    on.exit(setwd(cwd))

    ## The default return value for a success package building
    result <- pkg

    if (dry.run) {
        filename <- paste0(pkg, "_timing.txt")
        file.create(filename)
    } else {
        withCallingHandlers({
            suppressMessages(
                BiocManager::install(
                    pkg,
                    type = "source",
                    INSTALL_opts = "--build",
                    update = FALSE,
                    quiet = FALSE,
                    force = TRUE,
                    ## TODO: a successful install output isn't useful
                    keep_outputs = TRUE
                )
            )
        },
        error = function(e) {
            flog.error(
                "Error: package %s failed", pkg, name = "minibioc_install"
            )
            result <<- e
        },
        warning = function(e) {
            flog.error(
                "Error: package %s failed", pkg, name = "minibioc_install"
            )
            result <<- e
            tryInvokeRestart("muffleWarning")
        }
        )
    }
    result
}

#' @rdname minibioc_single
#'
#' @examplesIf interactive()
#' build_source_package(
#'     pkg = "~/bioc/BiocParallel",
#'     lib_path = local_library(),
#'     bin_path = local_src_repo(),
#'     log_path = local_src_log()
#' )
#'
#' @export
build_source_package <-
    function(pkg, lib_path, bin_path, log_path)
{
    .libPaths(c(lib_path, .libPaths()))

    log_file <- file.path(log_path, 'minibioc_build.log')
    flog.appender(appender.tee(log_file), name = 'minibioc_build')

    flog.info(
        "building source package: %s", basename(pkg), name = 'minibioc_build'
    )
    cwd <- setwd(bin_path)
    on.exit(setwd(cwd))

    ## The default return value for a success package building
    result <- pkg

    withCallingHandlers({
        suppressMessages(
            pkgbuild::build(
                path = pkg,
                dest_path = bin_path
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

#' Install and create binaries for packages parallely
#'
#' @description Install packages and create binaries using a BiocParallelParam
#'   for a specific bioconductor docker image. The minibioc_install function can
#'   be scaled to a large cluster to reduce times even further (in theory).
#'   Please note that this command may charge your Google billing account,
#'   beware of the charges.
#'
#' @param lib_path `character(1)` path where R package libraries are
#'     stored.
#'
#' @param bin_path `character(1)` path where R package binaries are
#'     stored.
#'
#' @param log_path `character(1)` path where R package binary build logs
#'     are stored.
#'
#' @param deps package dependecy graph as computed by `.pkg_dependencies()`.
#'
#' @param BPPARAM A `BiocParallelParam` object specifying how each
#'     level of the dependency graph will be parallelized. Use
#'     `SerialParam()` for debugging
#'
#' @importFrom BiocParallel bpiterate bpprogressbar SerialParam bpprogressbar<- SnowParam
#' @importFrom futile.logger flog.error flog.info flog.appender appender.file appender.tee
#'
#' @examplesIf interactive()
#' library(BiocParallel)
#' bpparam <- MulticoreParam(
#'     workers = 22, stop.on.error = FALSE, jobname = "minibioc_binaries"
#' )
#' ## First method:
#' ## Run with a pre-existing bucket with some packages.
#' ## This will update only the new packages
#' deps <- pkg_dependencies(binary_repo = local_bin_repo())
#' minibioc_install(
#'     lib_path = .libPaths()[1],
#'     bin_path = local_bin_repo(),
#'     log_path = local_bin_log(),
#'     deps = deps,
#'     dry.run = FALSE,
#'     BPPARAM = bpparam
#' )
#'
#' ## Second method:
#' ## Create a new google CRAN style bucket and populate with binaries.
#' gcloud_create_cran_bucket("gs://my-new-binary-bucket",
#'     "1.0", "3.11", secret = "/home/mysecret.json", public = TRUE)
#'
#' deps_new <- pkg_dependencies(binary_repo = "my-new-binary-bucket/1.0/3.11")
#'
#' minibioc_install(
#'     lib_path = "/host/library",
#'     bin_path = local_bin_repo(),
#'     log_path = local_bin_log(),
#'     deps = deps_new
#' )
#'
#' @export
minibioc_install <-
    function(lib_path, bin_path, log_path, deps, dry.run, BPPARAM = NULL)
{
    stopifnot(
        isScalarCharacter(lib_path) || is.null(lib_path),
        isScalarCharacter(bin_path),
        isScalarCharacter(log_path)
    )

    ## Only if BPPARAM is null, use SnowParam
    if (is.null(BPPARAM)) {
        BPPARAM <- BiocParallel::SnowParam(stop.on.error = FALSE)
    }
    ## disable the default progressbar
    progressbar_arg <- bpprogressbar(BPPARAM)
    bpprogressbar(BPPARAM) <- FALSE
    on.exit(bpprogressbar(BPPARAM) <- progressbar_arg, add = TRUE)

    ## Logging
    log_file <- file.path(log_path, 'minibioc_install.log')
    flog.appender(appender.tee(log_file), name = 'minibioc_install')
    flog.info(
        "%d packages to process ",
        length(deps),
        name = "minibioc_install"
    )

    error_file <- file.path(log_path, 'minibioc_errors.log')
    flog.appender(appender.tee(error_file), name = 'minibioc_errors')

    progress_file <- file.path(log_path, 'minibioc_progress.log')
    flog.appender(appender.tee(progress_file), name = 'minibioc_progress')

    ## Iterator function
    iter <- .dependency_graph_iterator_factory(
        deps,
        install_build_binary
    )

    result <- bpiterate(
        iter$ITER,
        iter$FUN,
        dry.run = dry.run,
        lib_path = lib_path,
        bin_path = bin_path,
        log_path = log_path,
        REDUCE = iter$REDUCE,
        init = c(), ## need to keep this as initial value for reducer
        BPPARAM = BPPARAM
    )
    result <- as.list(result)

    ## Logging to document how many packages failed and installed
    ## TRUE is success, FALSE is fail
    ## TODO: try to log excluded packages like canceR, and ChemmineOB
    flog.info(
        "%d built, %d succeeded, %d failed",
        length(deps),
        length(deps) - length(result),
        length(result),
        name = "minibioc_install"
    )

    if (length(result)) {
        flog.info(
            "Failed packages: %s",
            paste0(names(result), collapse = ", "),
            name = "minibioc_install"
        )
    }

    if (length(iter$this$failed)) {
        pkgs <- as.list(iter$this$failed)
        msg <- paste0(names(pkgs),
                      " failed for the reason: ",
                      as.character(pkgs))
        flog.error(msg, name = "minibioc_errors")
    }

    ## Create PACKAGES, PACKAGES.gz, PACAKGES.rds
    tools::write_PACKAGES(bin_path, addFiles = TRUE, verbose = TRUE)
    flog.info("PACKAGES files created", name = "minibioc_install")

    result
}

.get_artifact_paths <-
    function(version, volume_mount_path)
{
    list(
        lib_path = .create_artifact_dir(version, volume_mount_path, 'library'),
        bin_path = .create_artifact_dir(version, volume_mount_path, 'binary'),
        log_path = .create_artifact_dir(version, volume_mount_path, 'logs')
    )
}

.bin_artifact_paths <-
    function(base_repo_dir = minibioc_base_dir(), version)
{
    list(
        lib_path = local_library(
            base_repo_dir = base_repo_dir,
            version = version
        ),
        bin_path = local_bin_repo(
            base_repo_dir = base_repo_dir,
            version = version
        ),
        log_path = local_bin_log(
            base_dir = base_repo_dir,
            version = version
        )
    )
}
