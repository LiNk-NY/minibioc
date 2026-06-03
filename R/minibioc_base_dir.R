#' Get or set the minibioc base repository directory
#'
#' @param ... Additional arguments passed to `minibioc_set_cache`.
#' @param ask `logical(1)` Whether to prompt the user before creating the directory.
#'
#' @export
minibioc_base_dir <- function(..., ask = interactive()) {
    getOption(
        "minibioc.base_repo_dir",
        minibioc_set_cache(..., verbose = FALSE, ask = ask)
    )
}

#' @rdname minibioc_base_dir
#'
#' @param directory `character(1)` The directory path to use for cache/repository.
#' @param verbose `logical(1)` Whether to print a message when setting the directory.
#' @param ask `logical(1)` Whether to prompt the user before creating the directory.
#'
#' @importFrom BiocBaseUtils isScalarCharacter askUserYesNo
#' @export
minibioc_set_cache <- function(
    directory = file.path(Sys.getenv("HOME"), "minibioc"),
    verbose = TRUE,
    ask = interactive()
) {
    stopifnot(
        isScalarCharacter(directory)
    )

    if (!dir.exists(directory)) {
        if (ask) {
            qtxt <- sprintf(
                "Create minibioc repository at \n    %s",
                directory
            )
            isYES <- askUserYesNo(qtxt)
            if (!isYES)
                stop("'minibioc' repository not created. Use 'minibioc_set_cache'")
        }
        dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    }
    options(minibioc.base_repo_dir = directory)

    if (verbose)
        message("minibioc repository directory set to:\n    ", directory)

    invisible(directory)
}
