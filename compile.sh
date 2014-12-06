#!/bin/bash

#rm -r build
#rm -r cache

elm -m -o --src-dir=./src/elm src/elm/Main.elm

if [ $? -eq 0 ]
then

  cp -r ./src/imgs ./build

  mkdir -p ./build/js

  uglifyjs $HOME/.cabal/share/Elm-0.13/elm-runtime.js > ./build/js/elm-runtime.js

  for pathname in ./build/src/elm/*.js
  do
    filename="${pathname##*/}"
    uglifyjs "$pathname" > "./build/js/$filename"
  done

  cp ./src/index.html ./build/index.html
  uglifyjs ./src/htmlMain.js > ./build/js/htmlMain.js
  cp ./src/jquery-1.11.1.min.js ./build/js/jquery-1.11.1.min.js
  yui-compressor ./src/style.css > ./build/style.css

  rm -r ./build/src

fi