FROM rocker/shiny:3.6.1

RUN apt-get update \
	&& apt-get install -y \
	  curl \
	  libssl-dev \
	  glpk-utils \
	  libgdal-dev \
	  libxml2-dev \
	  libgeos-dev \
	  libproj-dev \
	  libglpk-dev \
	  libudunits2-dev \
	  libcurl4-openssl-dev 

RUN install2.r \
      --error \
      dplyr \
      tidyr \
      readr \
      plotly \
      shinyBS \
      heatmaply \
      stringr \
      fPortfolio \
      shinythemes \
      shinyWidgets \
      janitor \
      ggplot2 \
      DT \
      quantmod \
      PortfolioAnalytics \
      PerformanceAnalytics \
      shinycssloaders \
      purrr \
      broom \
      ROI \
      ROI.plugin.glpk \
      ROI.plugin.quadprog \
      tidyquant \
      shinyjs

COPY . /srv/shiny-server/diversified_crypto

RUN chmod -R 777 /srv/shiny-server/
