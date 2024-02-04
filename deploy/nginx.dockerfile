FROM alpine/git AS website
RUN mkdir -p /website
WORKDIR /website
RUN git clone --single-branch --branch gh-pages https://github.com/samcf/ogres.git .

FROM nginx:alpine
ARG SERVER_SOCKET_URL
ENV SERVER_SOCKET_URL=${SERVER_SOCKET_URL:-"http://backend:8090/ws"}
ARG VERSION
ENV VERSION=${VERSION:-"dev"}
COPY --from=website /website /usr/share/nginx/html
