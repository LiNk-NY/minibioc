library(minibioc)
library(biocViews)

## Point to source package directories
source_base_dir <- "~/bioc"

## gitcreds::gitcreds_set()
library(ReleaseLaunch)

## update all repositories to latest checkout
update_local_repos(
    repos_dir = source_base_dir, org = "Bioconductor",
    BPPARAM = BiocParallel::MulticoreParam(workers = 20L)
)

## create paths to local source packages
bioc_sub_pkgs <- file.path(
    source_base_dir, c(
        "SummarizedExperiment",
        "Biobase",
        "BiocGenerics",
        "DelayedArray",
        "GenomicRanges",
        "IRanges",
        "S4Vectors"
    )
)

## install all dependencies for building
remotes::install_local(
    bioc_sub_pkgs,
    dependencies = TRUE,
    repos = BiocManager::repositories(),
    force = TRUE
)

## install local sources for all packages
for (pkg in bioc_sub_pkgs)
    build_source_package(
        pkg = pkg,
        lib_path = local_library(),
        bin_path = local_src_repo(),
        log_path = local_src_log()
    )

## Install local binaries for all packages
for (pkg in bioc_sub_pkgs)
    install_build_binary(
        pkg = pkg,
        dry.run = FALSE,
        lib_path = local_library(),
        bin_path = local_bin_repo(),
        log_path = local_bin_log()
    )

## Create REPOSITORY files for the local source repository
write_REPOSITORY(
    local_src_repo(contrib.url = FALSE),
    contribPaths = c(
        "source" = "src/contrib"
    )
)

## Create REPOSITORY files for the local binary repository
write_REPOSITORY(
    local_bin_repo(contrib.url = FALSE),
    contribPaths = c(
        "linux.binary" = "src/contrib"
    )
)

## Create PACKAGES, PACKAGES.gz, PACAKGES.rds for the local source repository
tools::write_PACKAGES(local_src_repo(), addFiles = TRUE, verbose = TRUE)

## Create PACKAGES, PACKAGES.gz, PACAKGES.rds for the local binary repository
tools::write_PACKAGES(local_bin_repo(), addFiles = TRUE, verbose = TRUE)

## create vignettes directory for the local source repository
extractVignettes(
    reposRoot  = local_src_repo(contrib.url = FALSE),
    srcContrib = "src/contrib"
)

biocViews::extractCitations(
    reposRoot = local_src_repo(contrib.url = FALSE),
    srcContrib = "src/contrib",
    destDir = file.path(
        local_src_repo(contrib.url = FALSE),
        "citations"
    )
)

biocViews:::extractReadmes(
   reposRoot  = local_src_repo(contrib.url = FALSE),
   srcContrib = "src/contrib",
   destDir    = file.path(
       local_src_repo(contrib.url = FALSE),
       "readme"
   )
)

biocViews:::extractNEWS(
    reposRoot  = local_src_repo(contrib.url = FALSE),
    srcContrib = "src/contrib"
)

biocViews:::extractHTMLManuals(
    reposRoot  = local_src_repo(contrib.url = FALSE),
    srcContrib = "src/contrib",
    destDir = file.path(
        local_src_repo(contrib.url = FALSE),
        "manuals"
    )
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
