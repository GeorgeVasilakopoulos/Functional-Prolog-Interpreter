GHC = ghc
GHCFLAGS = #-Wall

# Directories
CORE_DIR = src
TOPDOWN_DIR = src/TopDown
PARSER_DIR = src/parser
BOTTOMUP_DIR = src/BottomUp
SOURCE_DIRS = $(CORE_DIR) $(TOPDOWN_DIR) $(PARSER_DIR) $(BOTTOMUP_DIR)

# Source Files
CORE_FILES = $(wildcard $(CORE_DIR)/*.hs)
TOPDOWN_FILES = $(wildcard $(TOPDOWN_DIR)/*.hs)
PARSER_FILES = $(wildcard $(PARSER_DIR)/*.hs)
BOTTOMUP_FILES = $(wildcard $(BOTTOMUP_DIR)/*.hs)

#Executables
TOPDOWN_EXE = ./topdown
BOTTOMUP_EXE = ./bottomup


$(TOPDOWN_EXE): $(CORE_FILES) $(PARSER_FILES) $(TOPDOWN_FILES)
	$(GHC) $(GHCFLAGS) -o $@ $^



clean:
	find . -type f -name '*.o' -delete
	find . -type f -name '*.hi' -delete
	rm $(TOPDOWN_EXE)