FROM haskell:9.2.7

WORKDIR .

COPY . .

RUN mkdir -p /opt/augslink

RUN cabal update
RUN cabal install
COPY dist-newstyle/x86_64-linux/ghc-9.2.7/augslink-0.1.0/x/augslink/build/augslink/augslink /opt/augslink

RUN npm install
RUN npm run build
COPY dist /opt/augslink/dist-static

CMD ["/opt/augslink/augslink", "-e", "prod", "-p", "dist-static"]
EXPOSE 8080
