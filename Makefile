all: hfe

ALL := $(wildcard src/*)

hfe: ${ALL}
	ghc -isrc -o hfe --make src/Main.hs
