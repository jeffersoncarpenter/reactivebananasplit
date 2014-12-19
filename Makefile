all:
	ghc -no-user-package-db -package-db .cabal-sandbox/*-packages.conf.d Main.hs 
