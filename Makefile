GHC = ghc
GHCFLAGS = -Wall

# Directories
SRCDIR = src

CORE = Types.hs

topdown: 
    ghc "$(SRCDIR)/Main.hs" 
