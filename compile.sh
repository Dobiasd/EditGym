#!/usr/bin/env bash

rm -r build

mkdir build
mkdir build/js

elm-make src/elm/Start.elm src/elm/Exercises.elm src/elm/Game.elm src/elm/Contact.elm src/elm/Help.elm src/elm/FAQ.elm src/elm/CreateExercise.elm src/elm/PersonalBestList.elm --output build/js/editgym_uncompressed.js

if [ $? -eq 0 ]
then

  cp ./src/.htaccess ./build
  cp -r ./src/imgs ./build
  cp -r ./src/exercises ./build

  uglifyjs build/js/editgym_uncompressed.js > build/js/editgym.js
  rm build/js/editgym_uncompressed.js

  cp ./src/index.html ./build/index.html
  uglifyjs ./src/htmlmain.js > ./build/js/htmlmain.js
  cp ./src/jquery-1.11.1.min.js ./build/js/jquery-1.11.1.min.js
  yui-compressor ./src/style.css > ./build/style.css

fi