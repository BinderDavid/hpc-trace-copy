HPC_ROOT=.

CABAL=runhaskell Setup.hs

.PHONY: all test build configure inplace-install clean

all:: configure build install

configure::
	$(CABAL) configure $(ARGS)

build:: 
	$(CABAL) build $(ARGS)

install::
	$(CABAL) install

inplace-install::
	$(CABAL) copy --destdir="inplace"

test:: inplace-install
	make -C tests test

clean::
	rm -Rf ./dist
	rm -f src/CachedFiles.hs

CACHED = fs/root.html \
	fs/footer.html \
	fs/header.html \
	fs/status.html \
	fs/code.html \
	fs/default.js \
	fs/default.css \
	fs/favicon.ico \

boot:: src/CachedFiles.hs

src/CachedFiles.hs: $(CACHED) includefile.pl
	echo "module CachedFiles where" 				 > $@
	perl ./includefile.pl root_html 	< fs/root.html 		>> $@
	perl ./includefile.pl footer_html 	< fs/footer.html 	>> $@
	perl ./includefile.pl header_html 	< fs/header.html 	>> $@
	perl ./includefile.pl code_html 	< fs/code.html 		>> $@
	perl ./includefile.pl status_html 	< fs/status.html 	>> $@
	perl ./includefile.pl default_js 	< fs/default.js 	>> $@
	perl ./includefile.pl default_css 	< fs/default.css 	>> $@
	perl ./includefile.pl favicon -bin 	< fs/favicon.ico 	>> $@
	perl ./includefile.pl progress -bin 	< fs/progressbar_green.gif \
									>> $@

