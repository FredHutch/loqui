FROM fredhutch/r-shiny-base:4.2.0

RUN R -e "install.packages(c('ari', 'text2speech', 'shinyWidgets'), repos='https://cran.rstudio.com/')"

ADD . /app

WORKDIR /app

CMD R -f app.R

