ARG MODE="release"

FROM node:alpine AS node
RUN mkdir -p /node
WORKDIR /node
COPY package.json package-lock.json ./
RUN npm ci
RUN mkdir -p web/release
COPY . .
RUN npm rebuild esbuild
RUN npx esbuild \
  --bundle \
  --minify \
  --loader:.svg=dataurl \
  --outfile=./web/release/ogres.app.css \
  ./src/main/ogres/app/resource/root.css

# Release build stage
# - Compiles the ClojureScript frontend to `/release/$VERSION` directory, defaulting to `/release/latest`
# - Copies `/release` assets such `icons.svg` and the ESBuild output of `ogres.app.css` to the same directory
FROM clojure:temurin-21-tools-deps-bookworm AS frontend-release
ARG VERSION
ARG SERVER_SOCKET_URL="wss://ogres.app/ws"
ENV VERSION=${VERSION:-"dev"} \
  OUTPUT_DIR="web/release/${VERSION}" \
  ASSET_PATH="release/${VERSION}"
WORKDIR /build
COPY . .
COPY --from=node /node/node_modules ./node_modules/
COPY --from=node /node/web/release/icons.svg /node/web/release/ogres* ./web/release/${VERSION}/
RUN clojure -M -m shadow.cljs.devtools.cli release app \
  --config-merge "{:closure-defines {ogres.app.const/VERSION \"${VERSION}\" ogres.app.const/PATH \"/release/${VERSION}\" ogres.app.const/SOCKET-URL \"${SERVER_SOCKET_URL}\"}}"

VOLUME ["/build"]

# Watch build stage (for development)
# - Compiles the ClojureScript frontend to the `/release` directory
# - Watches for changes in the source code and rebuilds automatically
# - Does not watch for CSS changes, only ClojureScript changes
FROM clojure:temurin-21-tools-deps-bookworm AS frontend-watch
ARG SERVER_SOCKET_URL
ENV SERVER_SOCKET_URL=${SERVER_SOCKET_URL:-"wss://ogres.app/ws"}
WORKDIR /build
COPY . .
COPY --from=node /node/node_modules ./node_modules/
COPY --from=node /node/web/release ./web/release/
EXPOSE 9630
CMD clojure -M -m shadow.cljs.devtools.cli watch app --config-merge "{:closure-defines {ogres.app.const/SOCKET-URL \"${SERVER_SOCKET_URL}\"}}"
VOLUME ["/build"]

# The ARG MODE determines which build stage to use
FROM frontend-${MODE}
