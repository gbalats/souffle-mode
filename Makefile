EMACS := emacs
CASK  := cask

CASKED_EMACS = $(CASK) exec $(EMACS)
DISTDIR := dist

project         := souffle-mode
project_version := $(shell $(CASK) version)
project_pkgdir  := $(shell $(CASK) package-directory)
project.tar     := $(wildcard $(DISTDIR)/$(project)-*.tar)
project_sources := $(patsubst %,souffle-%.el,mode core)
project_include := $(addprefix -l , $(project_sources))

export EMACS

define pkg_install_file
emacs --batch --eval "                                           \
  (progn                                                         \
     (require 'package)                                          \
     (add-to-list 'package-archives                              \
                  '(\"melpa\" . \"http://melpa.org/packages/\")) \
     (package-initialize)                                        \
     (unless package-archive-contents                            \
        (package-refresh-contents))                              \
     (package-install-file \"$1\" ))                             \
  "
endef

all: $(project)-pkg.el dist

deps: | $(project_pkgdir)

compile: deps
	$(CASK) build

run:
	$(CASKED_EMACS) -Q $(project_include)


# Cleaning targets

clean: clean-elc clean-dist
distclean: clean clean-deps

clean-dist:
	rm -rf $(DISTDIR)/

clean-elc:
	$(CASK) clean-elc

clean-deps:
	rm -rf .cask/


# Make tar package

dist: compile
	@mkdir -p $(DISTDIR)
	tar cf $(DISTDIR)/$(project)-$(shell $(CASK) version).tar $(shell $(CASK) files)
	# $(CASK) package $(DISTDIR)

.PHONY: all compile run \
	clean clean-deps clean-elc clean-dist \
	dist distclean deps


# Package descriptor file

$(project)-pkg.el: $(project).el Cask
	$(CASK) pkg-file


# Package directory

$(project_pkgdir):
	$(CASK) install


# Setup Cask

.PHONY: setup
setup: $(HOME)/.cask

$(HOME)/.cask:
	curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python


# Install package to emacs

.PHONY: install $(project.tar)
install: $(project.tar)

$(project.tar):
	$(call pkg_install_file,$(abspath .))


#---------------------------------------------------------------------
# Testing with Ecukes
#---------------------------------------------------------------------

.PHONY: test
test:
	$(CASK) exec ecukes
