# auxwire
Repository for the auxwire.link web server code and client-side code. The web server is written in Haskell. The client-side is written in HTML, CSS, and Typescript.

## The mission
Make it super easy to listen to music live with your friends; where you all take turns playing a song.

## Steps to run the web server locally

*These steps assume you have GHC, Cabal, and Node installed!*

1. The web server runs on https. Use [mkcert](https://github.com/FiloSottile/mkcert) to generate a cert and key... Place these two files in a directory at the root of the project... name this directory *tls* preferably.
   ```
   mkcert -install
   mkcert localhost 127.0.0.1
   ```
2. Install npm packages then compile/bundle client-side code. This project uses Webpack for this. Compilation of Typescript happens via webpack. This step creates an output directory called *dist-static*.
   ```
   npm install
   npm run build
   ```
3. Run webserver. Notice the args p,c, and k are referencing files created from step 1 and 2. The cabal v2-run command will install+build if necessary.
   ```
   cabal v2-run . -- -p dist-static -c tls/localhost+1.pem -k tls/localhost+1-key.pem
   ```