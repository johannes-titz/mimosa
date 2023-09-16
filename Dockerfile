FROM rocker/r-base:latest

MAINTAINER Johannes Titz "shiny@titz.science"
# Based on some guides

# getting system deps via: https://www.jumpingrivers.com/blog/shiny-auto-docker/?
# can we start with a minimum r-installation?
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# do we need this?
# system library dependency for the euler app
#RUN apt-get update && apt-get install -y \
#    libmpfr-dev \
#    cmake \
#    && rm -rf /var/lib/apt/lists/*

# mimosa app from github with all deps
RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_github('johannes-titz/mimosa')"

# expose port
EXPOSE 3838

# run mimosa
CMD ["R", "-e", "mimosa::run_app(port = 3838, host = '0.0.0.0')"]
