library(minibioc)
library(biocViews)

## Point to source package directories
source_base_dir <- "~/bioc"
bioc_sub_pkgs <- file.path(
    source_base_dir, c(
        "SummarizedExperiment", "Biobase", "BiocBaseUtils",
        "BiocGenerics", "DelayedArray", "GenomicRanges",
        "IRanges", "S4Vectors"
    )
)

## Install local binaries for a single package
for (pkg in bioc_sub_pkgs) {
    install_build_binary(
        pkg = pkg,
        dry.run = FALSE,
        lib_path = local_library(),
        bin_path = local_bin_repo(),
        log_path = local_bin_log()
    )
}

## Create REPOSITORY files for the local source and binary repositories
write_REPOSITORY(
    local_src_repo(contrib.url = FALSE),
    contribPaths = c(
        "source" = "src/contrib"
    )
)

write_REPOSITORY(
    local_bin_repo(contrib.url = FALSE),
    contribPaths = c(
        "linux.binary" = "src/contrib"
    )
)

## Create PACKAGES, PACKAGES.gz, PACAKGES.rds for the local binary repository
tools::write_PACKAGES(local_bin_repo(), addFiles = TRUE, verbose = TRUE)

## Create PACKAGES, PACKAGES.gz, PACAKGES.rds for the local source repository
tools::write_PACKAGES(local_src_repo(), addFiles = TRUE, verbose = TRUE)

## create vignettes directory for the local source repository
extractVignettes(
    reposRoot  = local_src_repo(contrib.url = FALSE),
    srcContrib = "src/contrib"
)

biocViews:::extractReadmes(
   reposRoot  = local_src_repo(contrib.url = FALSE),
   srcContrib = "src/contrib",
   destDir    = local_src_repo(contrib.url = FALSE)
)

biocViews:::extractNEWS(
    reposRoot  = local_src_repo(contrib.url = FALSE),
    srcContrib = "src/contrib",
    destDir    = local_src_repo(contrib.url = FALSE)
)

biocViews:::extractINSTALLfiles(
    reposRoot  = local_src_repo(contrib.url = FALSE),
    srcContrib = "src/contrib",
    destDir    = local_src_repo(contrib.url = FALSE)
)

write_VIEWS(
    reposRootPath = local_src_repo(contrib.url = FALSE),
    manifestFile = "~/bioc/manifest/software.txt",
    meatPath = "~/bioc/"
)
