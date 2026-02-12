###### APP

# Basisimage mit R
FROM rstudio/plumber AS app

# System-Dependencies installieren
# RUN apt-get update && apt-get install -y \
#     libcurl4-openssl-dev \
#     libssl-dev \
#     libxml2-dev \
#     && rm -rf /var/lib/apt/lists/*

# System-Dependencies installieren
RUN apt-get update && apt-get install -y \
    build-essential \
    g++ \
    make \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    && rm -rf /var/lib/apt/lists/*

# R-Pakete
RUN R -e "install.packages(c('jsonlite','dplyr','readr','stringr'), repos='https://cloud.r-project.org'); \
          if(!require(remotes)) install.packages('remotes', repos='https://cloud.r-project.org'); \
          remotes::install_github('dgrtwo/fuzzyjoin')"

# Arbeitsverzeichnis setzen
WORKDIR /app

# Dateien ins Image kopieren
COPY run.R /app/run.R
COPY plumber.R /app/plumber.R
COPY daten/ /app/daten/

# Standardport für plumber
EXPOSE 8000

# https://github.com/rstudio/plumber/blob/main/Dockerfile#L32C1-L33C1
# ENTRYPOINT []

# Container-Startbefehl, other options:
# CMD ["R", "-e", "pr <- plumber::pr(); api <- plumber::plumb('/app/plumber.R');pr <- pr$mount('/api', api); pr$run(host ='0.0.0.0', port = 8000)"]
# CMD ["R", "-e", "root <- plumber::pr();api <- plumber::Plumber$new('plumber.R');root$mount('/api', api);api$setDocs(TRUE);root$run(host='0.0.0.0', port=8000)"]
# CMD ["R", "-e", "api <- plumber::pr();api <- plumber::Plumber$new('plumber.R');root$mount('/api', api);api$setDocs(TRUE);root$run(host='0.0.0.0', port=8000)"]

# ENV PLUMBER_APIPATH='/api' PLUMBER_APIHOST='0.0.0.0'
ENV PLUMBER_APIHOST='0.0.0.0' PLUMBER_APIPATH='/api'
CMD ["/app/plumber.R"]

###### REVERSE PROXY/static pages
FROM caddy:latest AS reverse_proxy
COPY Caddyfile /etc/caddy/Caddyfile
COPY static /var/www
