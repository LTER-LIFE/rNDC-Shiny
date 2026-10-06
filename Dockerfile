# Image that runs the rNDC.Shiny app (the GUI to LTER-LIFE's NatureDataCube).
FROM rocker/shiny:latest

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

# 1. The rNDC package, which the app is built on. The branch can be overridden at build time
#    (`docker build --build-arg RNDC_REF=main .`). Its own dependencies are installed with it.
#    (The default CRAN repository of the rocker images provides binary packages.)
ARG RNDC_REF=texel26
RUN R -e "install.packages('remotes'); remotes::install_github('LTER-LIFE/rNDC', ref = '${RNDC_REF}', upgrade = 'never')"

# 2. The remaining dependencies, read from DESCRIPTION (including the GitHub-only leaflet.extras
#    from `Remotes:`). Copying only DESCRIPTION first keeps this layer cached while the code changes.
COPY DESCRIPTION /tmp/rNDC.Shiny/DESCRIPTION
RUN R -e "remotes::install_deps('/tmp/rNDC.Shiny', upgrade = 'never')"

# 3. The package itself: this installs the Shiny app and its resources (inst/app).
COPY . /tmp/rNDC.Shiny
RUN R CMD INSTALL --no-multiarch /tmp/rNDC.Shiny && rm -rf /tmp/rNDC.Shiny

# Port used by the app
EXPOSE 3838

# NDC_TOKEN (required), ADC_TOKEN (optional) and SHINY_APP_BASE_URL come from the environment (.env).
CMD ["R", "-e", "rNDC.Shiny::ndc_gui(host = '0.0.0.0', port = 3838)"]
