target: watch-app watch-tailwind
watch-app:
	ghcid -c 'cabal repl' \
      --reload=./app/Main.hs \
      -T Main.main \
      --restart=./board-game-selector-htmx.cabal

watch-tailwind: 
	npx tailwindcss -i ./src/input.css -o ./public/output.css --watch
