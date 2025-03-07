#' @export
minibioc_base_dir <- function(..., ask = interactive()) {
    getOption(
        "minibioc.base_repo_dir",
        setCache(..., verbose = FALSE, ask = ask)
    )
}

#' @importFrom BiocBaseUtils isScalarCharacter askUserYesNo
#' @export
setCache <- function(
        directory = normalizePath("~/minibioc"),
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
                stop("'minibioc' repository not created. Use 'setCache'")
        }
        dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    }
    options(minibioc.base_repo_dir = directory)

    if (verbose)
        message("minibioc repository directory set to:\n    ", directory)

    invisible(directory)
}
