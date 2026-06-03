#' Create a minibioc repository using Redis worker queue
#'
#' @description This function builds binaries for the given package dependency
#'   graph using RedisParam to parallelize builds across workers.
#'
#' @param bioc_version `character(1)` The version of the repository/Bioconductor.
#'
#' @param image_name `character(1)` The name of the Docker image used for building.
#'
#' @param volume_mount_path `character(1)` The base directory where the repository
#'   is mounted/created.
#'
#' @param cloud_id `character(1)` The cloud provider to use, one of `"local"`,
#'   `"gcp"`, or `"azure"`.
#'
#' @param build `character(1)` The build mode, one of `"_software"`, `"_update"`,
#'   or `"_timings"`.
#'
#' @param depth0 `logical(1)` Whether to only install/build packages with zero dependencies.
#'
#' @param dry.run `logical(1)` Whether to run a dry run.
#'
#' @param ultimate_pkg `character()` The ultimate package(s) to target.
#'
#' @param exclude_pkgs `character()` Packages to exclude from the dependency list.
#'
#' @examplesIf interactive()
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
    if (identical(cloud_id, "local"))
        local_sync_artifacts(
            artifacts = artifacts,
            repos = repos
        )
    else if (identical(cloud_id, "google"))
        ## PAIN POINT 3: Remove from this function
        ## all sync goes to Github actions
        cloud_sync_artifacts(
            secret = secret,
            artifacts = artifacts,
            repos = repos
        )

    ## ## Step 5: check if all workers were used
    check <- table(unlist(res))

    check
}
