GHC := ghc
GHCFLAGS := --make -O2

SRC := test.hs
TGT := $(basename $(SRC))

all: $(TGT)

%: %.hs Data/PicasaDB.hs Data/PicasaDB/Reader.hs
	$(GHC) $(GHCFLAGS) -o $@ $<
