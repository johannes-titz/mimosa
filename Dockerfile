#it would be interesting to try out the alpine image, but then dependencies
# must be handled manually; or arch
FROM rocker/r-ver:4

MAINTAINER Johannes Titz "shiny@titz.science"

# getting system deps via: https://www.jumpingrivers.com/blog/shiny-auto-docker/?
# just call glue_sys_reqs (see R/docker.R)
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
  cmake \
  libicu-dev \
  make \
  pandoc \
  zlib1g-dev \
  && apt-get clean

## needed?
## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# mimosa app from github with all deps
RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_github('johannes-titz/mimosa')"
RUN strip /usr/local/lib/R/site-library/*/libs/*.so

# update mimosa and deps if necessary
ADD "https://www.random.org/cgi-bin/randbyte?nbytes=10&format=h" skipcache
RUN R -e "remotes::install_github('johannes-titz/mimosa')"

# expose port
EXPOSE 3838

# run mimosa
CMD ["R", "-e", "mimosa::run_app(port = 3838, host = '0.0.0.0')"]