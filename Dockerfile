# Use a base image with R and Shiny
FROM rocker/shiny:latest

# Install system libraries needed by some R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libxml2-dev \
    libssl-dev \
    libgeos-dev \
    libgdal-dev \
    libproj-dev \
    libudunits2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages (leaflet.extras from GitHub — not on CRAN for R 4.5+).
# rNDC's own dependencies (terra, sf, rstac, ...) are resolved when installing it below.
RUN R -e "install.packages(c( \
    'shiny', 'leaflet', 'sf', 'dplyr', 'purrr', \
    'stringr', 'httr', 'geojsonsf', 'jsonlite', 'zip', 'here', 'terra', \
    'lubridate', 'tools', 'tibble', 'shinyjs', 'rstac', 'remotes' \
), repos='https://packagemanager.posit.co/cran/__linux__/noble/latest')" && \
    R -e "remotes::install_github('bhaskarvk/leaflet.extras')"

# Install the rNDC package (the branch can be overridden at build time).
ARG RNDC_REF=texel26
RUN R -e "remotes::install_github('LTER-LIFE/rNDC', ref = '${RNDC_REF}', upgrade = 'never')"

# Copy the Shiny app. There is no R package to install here: the app is run directly.
COPY inst/shiny/naturedatacube_app /app

# Expose the port Shiny uses
EXPOSE 3838

# SHINY_APP_BASE_URL (from .env) sets the proxy path; NDC_TOKEN and ADC_TOKEN are read by the app.
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
