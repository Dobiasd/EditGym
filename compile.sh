#!/bin/bash

rm -r build

mkdir build
mkdir build/js

elm-make src/elm/Start.elm --output build/js/Start.js
elm-make src/elm/Levels.elm --output build/js/Levels.js
elm-make src/elm/Game.elm --output build/js/Game.js
elm-make src/elm/Highscores.elm --output build/js/Highscores.js
elm-make src/elm/Contact.elm --output build/js/Contact.js
elm-make src/elm/Help.elm --output build/js/Help.js
elm-make src/elm/FAQ.elm --output build/js/FAQ.js

if [ $? -eq 0 ]
then

  cp -r ./src/imgs ./build

  mkdir -p ./build/js

  #uglifyjs $HOME/.cabal/share/Elm-0.13/elm-runtime.js > ./build/js/elm-runtime.js

  for pathname in ./build/js/*.js
  do
    filename="${pathname##*/}"
    uglifyjs "$pathname" > "$pathname.ugly"
    sleep 1
    rm "$pathname"
    mv "$pathname.ugly" "$pathname"
  done

  cp -R ./src/levels ./build

  cp ./src/index.html ./build/index.html
  uglifyjs ./src/htmlMain.js > ./build/js/htmlMain.js
  cp ./src/jquery-1.11.1.min.js ./build/js/jquery-1.11.1.min.js
  yui-compressor ./src/style.css > ./build/style.css

fi