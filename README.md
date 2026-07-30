# minibioc

`minibioc` is an R package for creating a **small, local Bioconductor-style package repository** from package source directories.

## What this package does

- Builds source tarballs for selected Bioconductor/R packages
- Resolves package dependencies and installs/builds binary artifacts
- Creates and updates CRAN-like repository metadata (e.g., `PACKAGES`)
- Generates HTML manual metadata/pages for package documentation
- Organizes local repository, library, and log directories
- Supports parallelized builds, including Redis-based worker queues

## What it is for

Use `minibioc` when you need to:

- Reproduce a subset of the Bioconductor repository locally
- Prepare internal/offline package repositories for testing or deployment
- Make testing changes to the [bioconductor.org](https://github.com/bioconductor/bioconductor.org) website easier by working against a local package repository
- Build package binaries for a specific Bioconductor release
- Manage incremental package build workflows and related logs/artifacts

In short, `minibioc` helps you bootstrap and maintain a minimal Bioconductor-compatible repository for development, CI, and distribution workflows.
