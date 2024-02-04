# Reference: https://practical.li/engineering-playbook/continuous-integration/docker/clojure-multi-stage-dockerfile/

FROM clojure:temurin-21-tools-deps-bookworm AS builder
RUN mkdir -p /build
WORKDIR /build
COPY ./ /build
RUN clojure -A:uberjar -m hf.depstar.uberjar OgreServer.jar

FROM eclipse-temurin:21-alpine AS final
RUN apk add --no-cache dumb-init

ARG UID=10001
RUN adduser \
    --disabled-password \
    --gecos "" \
    --home "/nonexistent" \
    --shell "/sbin/nologin" \
    --no-create-home \
    --uid "${UID}" \
    clojure

RUN mkdir -p /service && chown -R clojure. /service

USER clojure
WORKDIR /service
COPY --from=builder --chown=clojure:clojure /build/OgreServer.jar /service/OgreServer.jar

EXPOSE 8090

ENTRYPOINT ["/usr/bin/dumb-init", "--"]
CMD ["java", "-jar", "/service/OgreServer.jar", "-m", "ogres.server.core", "8090"]
