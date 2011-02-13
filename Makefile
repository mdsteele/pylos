# Makefile for Pylos.

#=============================================================================#

# Relative path to the directory containing the source code:
SRCDIR := src
# Relative path to the directory that will contain intermediate output files:
OUTDIR := out
# Relative path to the directory that will contain compiled executables:
BINDIR := bin
# Relative path to the directory that will contain documentation:
DOCDIR := doc
# Relative path to the Mac OS X application bundle:
APP_BUNDLE := Pylos.app

# Name of the main executable:
MAIN_EXE := pylos
# Name of the main executable:
TEST_EXE := pylos_test
# The title to use for the documentation:
DOC_TITLE := 'Pylos'

# Where can GHC's .h files be found?
GHC_C_INCLUDES_DIR := \
  /Library/Frameworks/GHC.framework/Versions/610/usr/lib/ghc-6.10.4/include/

# Flags we want to pass to all invokations of GHC:
GHC_FLAGS := -hidir $(OUTDIR) -odir $(OUTDIR) -stubdir $(OUTDIR) \
	     -i$(SRCDIR) -i$(OUTDIR) -Wall
# For release, add: -O2 -Werror

#=============================================================================#

.PHONY: main
main: $(OUTDIR)/libsdlboot.a $(OUTDIR)/Graphics/UI/SDL/Extras.hs \
                             $(OUTDIR)/System/MacOSX/Bundle.hs
	mkdir -p $(OUTDIR)
	ghc $(GHC_FLAGS) -no-hs-main --make HSMain \
	    -o $(APP_BUNDLE)/Contents/MacOS/$(MAIN_EXE) \
	    -lsdlboot -L$(OUTDIR) -optl-w

.PHONY: tests
tests:
	mkdir -p $(OUTDIR) $(BINDIR)
	ghc $(GHC_FLAGS) -main-is Test --make Test -o $(BINDIR)/$(TEST_EXE)

$(OUTDIR)/Graphics/UI/SDL/Extras.hs: $(SRCDIR)/Graphics/UI/SDL/Extras.hsc
	mkdir -p $(OUTDIR)/Graphics/UI/SDL/
	hsc2hs -o $@ $<

$(OUTDIR)/System/MacOSX/Bundle.hs: $(SRCDIR)/System/MacOSX/Bundle.hsc
	mkdir -p $(OUTDIR)/System/MacOSX/
	hsc2hs -o $@ $<

$(OUTDIR)/libsdlboot.a: $(OUTDIR)/c_main.o
	libtool -static -o $@ $<

$(OUTDIR)/c_main.o: $(SRCDIR)/c_main.c
	mkdir -p $(OUTDIR)
	gcc -c -I$(GHC_C_INCLUDES_DIR) -o $@ $<

#=============================================================================#

.PHONY: run
run: main
	$(APP_BUNDLE)/Contents/MacOS/$(MAIN_EXE)

.PHONY: test
test: tests
	$(BINDIR)/$(TEST_EXE)

.PHONY: docs
docs: $(OUTDIR)/Graphics/UI/SDL/Extras.hs
	mkdir -p $(DOCDIR)
	find $(SRCDIR) $(OUTDIR) -name '*.hs' -print0 | \
	    xargs -0 haddock --html --odir=$(DOCDIR) --title=$(DOC_TITLE)
	open $(DOCDIR)/index.html

.PHONY: clean
clean:
	rm -rf $(OUTDIR) $(BINDIR) $(DOCDIR)
	rm -f $(APP_BUNDLE)/Contents/MacOS/$(MAIN_EXE)

.PHONY: tidy
tidy:
	find $(SRCDIR) -name '*~' -print0 | xargs -0 rm
	find pov -name '*~' -print0 | xargs -0 rm

.PHONY: wc
wc:
	find $(SRCDIR) \( -name '*.hs' -or -name '*.hsc' \) -print0 | \
	    xargs -0 wc -l

.PHONY: fixme
fixme:
	ack "FIXME|TODO" $(SRCDIR)

#=============================================================================#
