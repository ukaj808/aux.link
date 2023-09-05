FROM haskell:9.2.7

RUN mkdir -p /opt/augslink

WORKDIR /opt/augslink

## Install pip to install yt-dlp
RUN apt-get update
RUN apt-get install -y python3
RUN curl -o get-pip.py https://bootstrap.pypa.io/get-pip.py 
RUN python3 get-pip.py
RUN python3 -m pip install -U yt-dlp

## Install node 18
RUN curl -fsSL https://deb.nodesource.com/setup_20.x | bash -
RUN apt-get install -y nodejs

## Install ffmpeg
RUN apt-get install -y ffmpeg 

## Haskell Build
RUN cabal update

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
COPY ./augslink.cabal .
RUN cabal build --only-dependencies -j4

## Now copy the rest of the source code (build)
COPY . .
RUN cabal install

RUN npm install
RUN npm run build

# todo
CMD ["augslink", "-p", "dist-static"]
EXPOSE 8080
