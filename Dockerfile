FROM haskell:8

# install database dependencies
RUN ["apt-get", "update"]
RUN ["apt-get", "-y", "install", "libpq-dev"]
RUN ["apt-get", "-y", "install", "pkg-config"]
RUN ["apt-get", "-y", "install", "libpcre3", "libpcre3-dev"]
RUN ["apt-get", "-y", "install", "build-essential"]


# Copy everything to docker ecosystem
WORKDIR /app/
COPY ["./*.cabal","./stack.yaml", "./"]
COPY ./app  ./app
COPY ./src/ ./src

RUN stack install --install-ghc -j1
RUN stack setup
RUN stack build -j1

CMD stack exec api-exe

EXPOSE 8080

