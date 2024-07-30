FROM fredhutch/r-shiny-base:4.4.0

ARG GITHUB_PAT

RUN echo break cache0
RUN apt-get --allow-releaseinfo-change update -y

RUN apt-get install -y usrmerge
RUN apt-get install -y libpoppler-cpp-dev ffmpeg
RUN apt-get install -y build-essential zlib1g-dev libncurses5-dev libgdbm-dev libnss3-dev libssl-dev libsqlite3-dev libreadline-dev libffi-dev curl libbz2-dev libreoffice default-jdk

RUN curl -LO https://www.python.org/ftp/python/3.9.1/Python-3.9.1.tgz
RUN tar -xf Python-3.9.1.tgz
RUN rm Python-3.9.1.tgz

WORKDIR  Python-3.9.1

RUN ./configure --enable-optimizations

RUN make -j `nproc`

RUN make altinstall

RUN python3.9 -m ensurepip
RUN python3.9 -m pip install pandas==2.0.3


RUN R -e "install.packages(c('gargle', 'googlesheets4', 'remotes', 'pdftools', 'tidyr', 'text2speech', 'shinyWidgets', 'aws.polly', 'shinyjs', 'blastula', 'promises', 'future', 'ipc', 'shinyFeedback'), repos='https://cran.rstudio.com/')"

# The latest version of tts as of 10/23/2023, 0.18,2, has a 
# bug: https://github.com/coqui-ai/TTS/issues/3074
# So we are using an older version until that is fixed.
# But we should pin to the latest version once that is fixed.
RUN python3.9 -m pip install TTS==0.18.1


# If this argument is supplied with a unique value, subsequent
# RUN steps will not be cached. See https://stackoverflow.com/a/49772666/470769
# We do this to ensure that we always get the latest packages from github.
ARG CACHEBUST=1


# TODO change this when PR is merged and ari is updated in CRAN:
RUN R -e 'remotes::install_github("jhudsl/text2speech", upgrade = "never")'
RUN R -e 'remotes::install_github("jhudsl/ari", "dev", upgrade = "never")'
RUN R -e 'remotes::install_github("fhdsl/gsplyr", upgrade = "never")'
RUN R -e 'remotes::install_github("fhdsl/ptplyr", upgrade = "never")'

# download tts models
RUN tts --text "download jenny"  --model_name tts_models/en/jenny/jenny --out_path /tmp/jenny.wav

RUN tts --text "download tacotron"  --vocoder_name "vocoder_models/en/ljspeech/univnet" --model_name tts_models/en/ljspeech/tacotron2-DDC_ph --out_path /tmp/tacotron.wav

RUN rm /tmp/jenny.wav /tmp/tacotron.wav 

# make sure all packages are installed
# because R does not fail when there's an error installing a package.
RUN R -e 'if(!all(commandArgs(TRUE) %in% installed.packages()[,"Package"])) q("no", 1)' --args remotes pdftools tidyr text2speech shinyWidgets aws.polly ari shinyjs blastula googlesheets4 gargle promises future ipc shinyFeedback gsplyr ptplyr 

RUN mkdir -p /private/

RUN ln -s /tmp /private/

ADD . /app

WORKDIR /app

ADD .secrets /app/.secrets

EXPOSE 3838

ENV LD_LIBRARY_PATH=/usr/lib/libreoffice/program

CMD R -f app.R

