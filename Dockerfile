# syntax=docker/dockerfile:1

ARG R_VERSION=4.6.1
FROM rocker/r-ver:${R_VERSION}

LABEL org.opencontainers.image.authors="Johannes Titz <shiny@titz.science>" \
      org.opencontainers.image.licenses="AGPL-3.0-only" \
      org.opencontainers.image.source="https://github.com/johannes-titz/mimosa"

# Keep dependency installation cacheable when application code changes.
WORKDIR /tmp/mimosa
COPY DESCRIPTION ./

# rocker/r-ver uses Posit Package Manager binaries on amd64. Removing download
# caches and stripping shared objects keeps the installed library compact.
RUN install2.r --error --skipinstalled --ncpus -1 \
      foreign \
      insight \
      lme4 \
      mlmRev \
      shiny \
      shinydashboard \
      shinyjs \
      sjPlot \
    && rm -rf /tmp/downloaded_packages \
    && find /usr/local/lib/R/site-library -type f -path '*/libs/*.so' \
      -exec strip --strip-unneeded '{}' +

# Install the package from this checkout, rather than downloading and then
# reinstalling the GitHub version with a random cache-busting URL.
COPY . ./
RUN R CMD INSTALL --no-multiarch . \
    && Rscript -e 'stopifnot(requireNamespace("mimosa", quietly = TRUE))' \
    && rm -rf /tmp/mimosa /tmp/downloaded_packages

# The application does not need root privileges at runtime.
RUN useradd --create-home --uid 10001 --shell /usr/sbin/nologin mimosa
USER mimosa
WORKDIR /home/mimosa

EXPOSE 3838
CMD ["R", "--no-save", "-e", "mimosa::run_app(port = 3838, host = '0.0.0.0')"]
