GHC := ghc
GHCFLAGS := --make -O2

CXXFLAGS = -Wall -Wextra -g -O3 -std=c++0x

SRC := pmpDB.hs thumbIndex.hs
TGT := $(basename $(SRC))

all: $(TGT) splitThumbs

%: %.hs Data/PicasaDB.hs Data/PicasaDB/Reader.hs
	$(GHC) $(GHCFLAGS) -o $@ $<

%: %.cpp
	$(CXX) $(CXXFLAGS) -o $@ $< $(LDFLAGS)
