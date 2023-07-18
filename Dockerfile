FROM rocker/shiny:latest
LABEL maintainer="USER <user@example.com>"
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    sudo \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    r-cran-v8 \
    libv8-dev \
    net-tools \
    libprotobuf-dev \
    protobuf-compiler \
    libjq-dev \
    libudunits2-0 \
    libudunits2-dev \
    libgdal-dev \
    libssl-dev
    && rm -rf /var/lib/apt/lists/*

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# copy necessary files
## renv.lock file
COPY /GeneApp/renv.lock ./renv.lock
# install renv & restore packages
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::restore()'
RUN Rscript -e 'BiocManager::install("maftools")'

## app folder
COPY /GeneApp ./GeneApp
#create user groups
RUN addgroup --system app \
    && adduser --system --ingroup app app
# expose port
EXPOSE 3838
# run app on container start
CMD ["R", "-e", "GeneApp::run_geneapp()"]
