# Base image: R + Shiny
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
    gnupg2 curl \
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
RUN echo '#!/bin/bash\n/usr/bin/google-chrome --no-sandbox $*' > /usr/local/bin/google-chrome && \
    chmod u+x /usr/local/bin/google-chrome

# ----------------------------
# Environment variables for headless Chrome / Pagedown
# ----------------------------
ENV CHROME_BIN=/usr/local/bin/google-chrome
ENV PAGEDOWN_CHROMIUM=/usr/local/bin/google-chrome
ENV PUPPETEER_SKIP_CHROMIUM_DOWNLOAD=true

# ----------------------------
# Install R packages needed by the app
# ----------------------------
RUN R -e "install.packages(c('shiny','shinydashboard','readxl','dplyr','tidyr','lubridate','stringr','purrr','gt','htmltools','pagedown','webshot2','rmarkdown'), repos='https://cloud.r-project.org')"

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
# Switch to shiny user
# ----------------------------
USER shiny

# ----------------------------
# Run the Shiny app
# ----------------------------
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/app.R', host='0.0.0.0', port=3838)"]
