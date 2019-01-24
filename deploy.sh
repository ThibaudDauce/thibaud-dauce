stack build && stack exec thibaud rebuild
npm run prod
rsync -Pr --delete _site/ thibaud.dauce.fr:/var/www/thibaud.dauce.fr/
