# syntax=docker/dockerfile:1
# Image that runs the rNDC.Shiny app (the GUI to LTER-LIFE's NatureDataCube).
# The base image is pinned (R version and digest), so that a rebuild gives the same R and system libraries. To
# update it, look up the digest of the new tag (`docker buildx imagetools inspect rocker/shiny:<tag>`).
FROM rocker/shiny:4.6.1@sha256:679ac44ec3d502474d3d55d1ea103528abd88fd32812f851bf388d4dfe5777ed

# System libraries needed by the spatial R packages (sf, terra, ...)
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libxml2-dev \
    libssl-dev \
    libgeos-dev \
    libgdal-dev \
    libproj-dev \
    libudunits2-dev \
    && rm -rf /var/lib/apt/lists/*

# Installing from GitHub (rNDC) uses the GitHub API, which limits anonymous requests. A token
# lowers the risk of a failing build; it is optional and is not kept in the image:
#   docker build --secret id=github_pat,env=GITHUB_PAT .
# `install.packages()` only warns when a package is not available, so each step ends with a check.

# 1. The rNDC package, which the app is built on, at a fixed commit (a branch would change under the build). At a
#    release use the tag of the rNDC release instead (e.g. `v0.5.0`). The reference can be overridden at build time
#    (`docker build --build-arg RNDC_REF=main .`). Its own dependencies are installed with it.
#    (The default CRAN repository of the rocker images provides binary packages.)
ARG RNDC_REF=c0afe00a4bb267b53c3cb3b208c609ec732d4bfd
RUN --mount=type=secret,id=github_pat,required=false <<INSTALL
set -e
if [ -s /run/secrets/github_pat ]; then export GITHUB_PAT="$(cat /run/secrets/github_pat)"; fi
R -q -e "install.packages('remotes'); remotes::install_github('LTER-LIFE/rNDC', ref = '${RNDC_REF}', upgrade = 'never'); stopifnot(requireNamespace('rNDC', quietly = TRUE))"
INSTALL

# 2. The remaining dependencies, read from DESCRIPTION. Copying only DESCRIPTION first keeps this layer cached
#    while the code changes.
COPY DESCRIPTION /tmp/rNDC.Shiny/DESCRIPTION
RUN --mount=type=secret,id=github_pat,required=false <<INSTALL
set -e
if [ -s /run/secrets/github_pat ]; then export GITHUB_PAT="$(cat /run/secrets/github_pat)"; fi
R -q -e "remotes::install_deps('/tmp/rNDC.Shiny', upgrade = 'never'); pkgs <- c('shiny', 'leaflet.extras', 'callr', 'sf', 'terra'); missing <- pkgs[!vapply(pkgs, requireNamespace, NA, quietly = TRUE)]; if (length(missing)) stop('not installed: ', paste(missing, collapse = ', '))"
INSTALL

# 3. The package itself: this installs the Shiny app and its resources (inst/app). The last command fails the
#    build if the package cannot be loaded.
COPY . /tmp/rNDC.Shiny
RUN R CMD INSTALL --no-multiarch /tmp/rNDC.Shiny && rm -rf /tmp/rNDC.Shiny \
    && R -q -e "library(rNDC.Shiny); stopifnot(is.function(ndc_gui), is.function(ndc_app))"

# Port used by the app
EXPOSE 3838

# NDC_TOKEN (required), ADC_TOKEN (optional) and SHINY_APP_BASE_URL come from the environment (.env).
# The app runs retrievals in background R processes, up to NDC_WORKERS (default 4) at a time, each using some
# hundreds of MB: set NDC_WORKERS to what the container's memory and CPUs allow.
CMD ["R", "-e", "rNDC.Shiny::ndc_gui(host = '0.0.0.0', port = 3838)"]
