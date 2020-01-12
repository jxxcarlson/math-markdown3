date +"%H:%M:%S"

if [ $1 = "-d" ]
then
  elm make  --debug src/Main.elm --output=public/main.js
else
  elm make  --optimize src/Main.elm --output=public/main.js
fi


