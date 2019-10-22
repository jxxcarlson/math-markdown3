if [ $1 = "-d" ]
then
  elm make  --debug src/Main.elm --output=public/Main.js
else
  elm make  --optimize src/Main.elm --output=public/Main.js
fi
