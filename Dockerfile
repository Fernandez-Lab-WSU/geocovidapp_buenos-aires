FROM rocker/r-base:4.4.0

# Instala librerías del sistema necesarias
RUN apt-get update && apt-get install -y --no-install-recommends \
    gdal-bin \
    libgdal-dev \
    libgeos-dev \
    libgeos++-dev \
    libicu-dev \
    libpng-dev \
    libproj-dev \
    libsqlite3-dev \
    libssl-dev \
    libudunits2-dev \
    make \
    pandoc \
    zlib1g-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    libpq-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libtiff-dev \
    libjpeg-dev \
    libgl1-mesa-dev \
    libgmp-dev \
    && rm -rf /var/lib/apt/lists/*

# Crear usuario no-root
RUN addgroup --system app && adduser --system --ingroup app --home /home/app app
RUN mkdir -p /home/app && chown app:app /home/app

WORKDIR /home/app

# Copia el paquete completo
COPY . .

# Cambia permisos
RUN chown app:app -R /home/app
USER app

# Instala renv y restaura dependencias
RUN R -e "install.packages('renv', repos = 'https://cloud.r-project.org')"
RUN R -e "options(repos = c(CRAN='https://cloud.r-project.org')); renv::restore(prompt=FALSE)"

# Instala el paquete local
RUN R CMD INSTALL .

# Exponer puerto
EXPOSE 3838

# Ejecutar la app desde inst/app

CMD ["R", "-e", "geocovidapp::run_app(demo = TRUE, port = 3838, host = '0.0.0.0')"]
