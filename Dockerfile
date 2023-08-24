FROM haskell:9.2.7
RUN npm run 
ENTRYPOINT ["pandoc"]
