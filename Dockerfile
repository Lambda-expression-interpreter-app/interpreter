FROM haskell:9.6.4
WORKDIR /app
RUN cabal update && cabal install --lib containers-0.6.7
RUN cabal install --lib scotty
RUN cabal install --lib text
RUN cabal install --lib utf8-string-1.0.2
COPY Main.hs ./Main.hs
COPY Interpreter.hs ./Interpreter.hs
COPY Stack.hs ./Stack.hs
COPY Makefile ./Makefile
RUN make build
CMD ["make", "run"]