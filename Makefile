GHC := ghc
GHCFLAGS := --make -O2

SRC := test.hs
TGT := $(basename $(SRC))

all: $(TGT)

%: %.hs
	$(GHC) $(GHCFLAGS) -o $@ $<
