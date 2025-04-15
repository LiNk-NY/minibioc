#' @name minibioc_single_package
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
#' @returns `install_binary_package()` returns invisibly
#'
#' @examples
#' install_binary_package(
#'     pkg = "BiocParallel",
#'     lib_path = NULL,
#'     bin_path = local_bin_repo(),
#'     log_path = local_bin_log()
#' )
#' @export
install_binary_package <-
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

#' @rdname minibioc_single_package
#'
#' @examples
#' repo_src_path <- "/home/rstudio/minibioc/packages/3.21/bioc/"
#' build_source_package(
#'     pkg = "~/bioc/BiocParallel",
#'     lib_path = NULL,
#'     bin_path = utils::contrib.url(repo_src_path),
#'     log_path = local_src_log()
#' )
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
#' @param lib_path character() path where R package libraries are
#'     stored.
#'
#' @param bin_path character() path where R package binaries are
#'     stored.
#'
#' @param log_path character() path where R package binary build logs
#'     are stored.
#'
#' @param deps package dependecy graph as computed by
#'     `.pkg_dependencies()`.
#'
#' @param BPPARAM A `BiocParallelParam` object specifying how each
#'     level of the dependency graph will be parallelized. Use
#'     `SerialParam()` for debugging
#'
#' @importFrom BiocParallel bpiterate bpprogressbar SerialParam
#'   `bpprogressbar<-` SnowParam
#'
#' @importFrom futile.logger flog.error flog.info flog.appender
#'     appender.file appender.tee
#'
#' @examples
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
        BPPARAM <- SnowParam(stop.on.error = FALSE)
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
        install_binary_package
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
        lib_path = NULL,
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

#' @examples
#' minibioc_redis(
#'     build = "_software",
#'     ultimate_pkg = "IRanges",
#'     exclude_pkgs = c("canceR", "ChemmineOB", "flowCore")
#' )
#'
#' @export
minibioc_redis <- function(
    bioc_version = BiocManager::version(),
    image_name = "bioconductor_docker",
    volume_mount_path = minibioc_base_dir(),
    cloud_id = c("local", "gcp", "azure"),
    build = c("_software", "_update", "_timings"),
    depth0 = FALSE,
    dry.run = TRUE,
    ultimate_pkg = character(),
    exclude_pkgs = character()
) {
    cloud_id <- match.arg(cloud_id)

    if (!identical(cloud_id, "local"))
        artifacts <- .get_artifact_paths(bioc_version, volume_mount_path)
    else
        artifacts <- .bin_artifact_paths(
            base_repo_dir = volume_mount_path, version = bioc_version
        )

    repos <- .repos(bioc_version, image_name, cloud_id = cloud_id)

    Sys.setenv(REDIS_HOST = Sys.getenv("REDIS_SERVICE_HOST"))
    Sys.setenv(REDIS_PORT = Sys.getenv("REDIS_SERVICE_PORT"))

    if (identical(cloud_id, "local")) {
        local_create_cran_bucket(
            image_name = image_name,
            version = bioc_version,
            bucket = volume_mount_path
        )
    } else if (identical(cloud_id, "google")) {
        ## Secret key to access bucket on google
        ## PAIN point 1: Also not needed
        secret <- "/home/key.json"

        ## Step 0: Create a bucket if you need to
        ## PAIN POINT 2: Creation of new buckets
        ## Do it via github actions
        gcloud_create_cran_bucket(
            folder = image_name,
            bioc_version = bioc_version,
            secret = secret, public = TRUE
        )
    } else {
        stop("'azure' cloud_id not implemented yet")
    }

    ## Step. 2 : Load deps and installed packages
    ## remove exclude packages
    deps <- pkg_dependencies(
        bioc_version, build = build,
        binary_repo = repos$binary,
        ultimate_pkg = ultimate_pkg,
        exclude = exclude_pkgs
    )

    if (depth0)
        deps <- deps[lengths(deps) == 0L]
    ## Step 3: Run minibioc_install so package binaries are built
    BPPARAM <- RedisParam(
        jobname = "binarybuild", is.worker = FALSE,
        progressbar = TRUE, stop.on.error = FALSE
    )

    res <- minibioc_install(
        lib_path = artifacts$lib_path,
        bin_path = artifacts$bin_path,
        log_path = artifacts$log_path,
        dry.run = dry.run,
        deps = deps, BPPARAM = BPPARAM
    )

    ## Stop RedisParam - This should stop all work on workers
    rpstopall(BPPARAM)

    ##  Step 4: Sync all artifacts produced, binaries, logs
    if (identical(cloud_id, "local")) {
        local_sync_artifacts(
            artifacts = artifacts,
            repos = repos
        )
    } else if (identical(cloud_id, "google")) {
        ## PAIN POINT 3: Remove from this function
        ## all sync goes to Github actions
        cloud_sync_artifacts(
            secret = secret,
            artifacts = artifacts,
            repos = repos
        )
    }

    ## ## Step 5: check if all workers were used
    check <- table(unlist(res))

    check
}
