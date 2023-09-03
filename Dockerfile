FROM haskell:9.2.7

## For pip > yt-dlp
RUN apt-get update
RUN apt-get install -y python3
RUN curl -o get-pip.py https://bootstrap.pypa.io/get-pip.py 
RUN python3 get-pip.py
RUN python3 -m pip install -U yt-dlp

## Install node 18
RUN curl -fsSL https://deb.nodesource.com/setup_18.x | bash -
RUN apt-get install -y nodejs

RUN apt-get install -y ffmpeg 

RUN mkdir -p /opt/augslink
RUN mkdir -p /opt/augslink/tls

WORKDIR /opt/augslink

COPY . .

RUN cabal update
## Should cache!
RUN cabal build --only-dependencies -j4
RUN cabal install

RUN npm install
RUN npm run build

# todo
CMD ["augslink", "-e", "local", "-p", "dist-static"]
EXPOSE 8080
