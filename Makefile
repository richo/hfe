all: hfe

ALL := $(wildcard src/*)

hfe: ${ALL}
	ghc -o hfe --make src/Main.hs
