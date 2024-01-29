ARG MODE="release"

FROM node:alpine AS node
RUN mkdir -p /node
WORKDIR /node
COPY package.json package-lock.json /node/
RUN npm install

FROM clojure:temurin-21-tools-deps-bookworm AS frontend-release
ARG VERSION="dev"
ARG SERVER_SOCKET_URL="wss://ogres.app/ws"
VOLUME /build
WORKDIR /build
COPY --from=node /node/node_modules /build/node_modules
COPY ./ /build
RUN clojure -M -m shadow.cljs.devtools.cli release app --config-merge "{:closure-defines {ogres.app.const/VERSION \"${VERSION}\" ogres.app.const/PATH \"/release/${VERSION}\" ogres.app.const/SOCKET-URL \"${SERVER_SOCKET_URL}\"}}"

FROM clojure:temurin-21-tools-deps-bookworm AS frontend-watch
ARG VERSION
ENV VERSION=${VERSION:-"dev"}
ARG SERVER_SOCKET_URL
ENV SERVER_SOCKET_URL=${SERVER_SOCKET_URL:-"wss://ogres.app/ws"}
VOLUME /build
WORKDIR /build
COPY --from=node /node/node_modules /build/node_modules
COPY ./ /build
EXPOSE 9630
CMD clojure -M -m shadow.cljs.devtools.cli watch app --config-merge "{:closure-defines {ogres.app.const/VERSION \"${VERSION}\" ogres.app.const/PATH \"/release/${VERSION}\" ogres.app.const/SOCKET-URL \"${SERVER_SOCKET_URL}\"}}"

FROM frontend-${MODE}
