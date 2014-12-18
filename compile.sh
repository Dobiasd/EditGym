#!/usr/bin/env bash

rm -r build

mkdir build
mkdir build/js

elm-make src/elm/Start.elm src/elm/Exercises.elm src/elm/Game.elm src/elm/Highscores.elm src/elm/Contact.elm src/elm/Help.elm src/elm/FAQ.elm src/elm/CreateExercise.elm src/elm/Newsletter.elm --output build/js/Bundle_uncompressed.js

if [ $? -eq 0 ]
then

  cp -r ./src/imgs ./build
  cp -R ./src/exercises ./build

  uglifyjs build/js/Bundle_uncompressed.js > build/js/Bundle.js
  rm build/js/Bundle_uncompressed.js

  cp ./src/index.html ./build/index.html
  uglifyjs ./src/htmlMain.js > ./build/js/htmlMain.js
  cp ./src/jquery-1.11.1.min.js ./build/js/jquery-1.11.1.min.js
  yui-compressor ./src/style.css > ./build/style.css

fi