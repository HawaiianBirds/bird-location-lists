# Base image: R + Shiny (now simplified)
FROM rocker/shiny:latest

# ----------------------------
# Install system dependencies
# ----------------------------
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev libxml2-dev libssl-dev \
    libfontconfig1 libfreetype6 libpng-dev libtiff5-dev libjpeg-dev \
    libgtk2.0-0 libx11-xcb1 libxcomposite1 libxcursor1 libxdamage1 \
    libxext6 libxfixes3 libxi6 libxrandr2 libxrender1 libxtst6 \
    ca-certificates fonts-liberation fonts-dejavu-core fonts-dejavu-extra \
    xdg-utils wget \
    gnupg2 curl git \
    texlive texlive-latex-base texlive-latex-extra texlive-fonts-recommended \
    && rm -rf /var/lib/apt/lists/*


# ----------------------------
# Install Google Chrome Stable
# ----------------------------
RUN wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add - && \
    sh -c 'echo "deb http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google.list' && \
    apt-get update -qq && \
    apt-get install -y google-chrome-stable && \
    rm -rf /var/lib/apt/lists/*

# ----------------------------
# Shim to run Chrome with --no-sandbox
# ----------------------------
RUN echo '#!/bin/bash\nexec /usr/bin/google-chrome --no-sandbox "$@"\n' > /usr/local/bin/google-chrome && \
    chmod a+rx /usr/local/bin/google-chrome

# ----------------------------
# Environment variables for headless Chrome / Pagedown
# ----------------------------
ENV CHROME_BIN=/usr/local/bin/google-chrome \
    PAGEDOWN_CHROMIUM=/usr/local/bin/google-chrome \
    PAGEDOWN_CHROME=/usr/local/bin/google-chrome \
    CHROMOTE_CHROME=/usr/local/bin/google-chrome \
    PUPPETEER_SKIP_CHROMIUM_DOWNLOAD=true


# ----------------------------
# Install R packages needed by the app
# ----------------------------
# Install CRAN pkgs (including remotes)
RUN R -e "install.packages(c('shiny','shinydashboard','readxl','dplyr','tidyr','lubridate','stringr','purrr','htmltools','pagedown','webshot2','rmarkdown','V8','remotes'), repos='https://cloud.r-project.org')"

RUN R -e "remotes::install_git('https://github.com/rstudio/juicyjuice.git'); remotes::install_git('https://github.com/rstudio/gt.git')"

# ----------------------------
# Copy the Shiny app
# ----------------------------
COPY app.R /srv/shiny-server/
COPY data/ /srv/shiny-server/data

# ----------------------------
# Fix permissions
# ----------------------------
RUN mkdir -p /srv/shiny-server/app_cache && \
    chown -R shiny:shiny /srv/shiny-server

# ----------------------------
# Expose Shiny port
# ----------------------------
EXPOSE 3838

# ----------------------------
# Switch to shiny user & set working dir
# ----------------------------
USER shiny
WORKDIR /srv/shiny-server

# ----------------------------
# Single CMD: run app, respect PORT (Render)
# ----------------------------
CMD ["R", "-e", "port <- as.numeric(Sys.getenv('PORT', '3838')); shiny::runApp('.', host='0.0.0.0', port=port)"]

