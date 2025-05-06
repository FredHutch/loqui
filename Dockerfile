FROM sc-registry.fredhutch.org/loqui:temp
RUN rm -rf /app
ADD . /app

WORKDIR /app

ADD .secrets /app/.secrets
