FROM fredhutch/r-shiny-base:4.2.0

RUN apt-get --allow-releaseinfo-change update -y

RUN apt-get install -y libpoppler-cpp-dev ffmpeg

RUN echo break cache


RUN R -e "install.packages(c('remotes', 'pdftools', 'tidyr', 'text2speech', 'shinyWidgets', 'aws.polly'), repos='https://cran.rstudio.com/')"

# TODO change this when PR is merged and ari is updated in CRAN:
RUN R -e 'remotes::install_github("jhudsl/text2speech", upgrade = "never")'
RUN R -e 'remotes::install_github("jhudsl/ari", "ariExtra-immigration", upgrade = "never")'

# make sure all packages are installed
# because R does not fail when there's an error installing a package.
RUN R -e 'if(!all(commandArgs(TRUE) %in% installed.packages()[,"Package"])) q("no", 1)' --args remotes pdftools tidyr text2speech shinyWidgets aws.polly ari


ADD . /app

WORKDIR /app

CMD R -f app.R

