FROM rocker/shiny:latest
LABEL maintainer="USER <mazzara.1742740@studenti.uniroma1.it>"
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libbz2-dev \
    liblzma-dev \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    r-cran-v8 \
    libv8-dev \
    net-tools \
    libharfbuzz-dev \
    libfribidi-dev \
    libprotobuf-dev \
    protobuf-compiler \
    libjq-dev \
    libudunits2-0 \
    libudunits2-dev \
    libgdal-dev \
    libssl-dev

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# copy necessary files
## renv.lock file
COPY /renv.lock ./renv.lock
# install renv & restore packages
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::restore()'
#RUN Rscript -e 'BiocManager::install("maftools")'
#RUN Rscript -e 'install.packages("maftools")'
## app folder
COPY / ./

#create user groups
RUN addgroup --system app \
    && adduser --system --ingroup app app
# expose port
EXPOSE 3838
# run app on container start
#CMD ["R", "-e", "geneApp::run_geneapp()"]
#CMD ["R", "-e", "devtools::load_all(".")"]
CMD ["R", "-e", "shiny::runApp('/inst/shinyapps/shiny_geneApp', host = '0.0.0.0', port = 3838)"]

