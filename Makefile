CONFIGUREFLAGS = --enable-ui --enable-tests --disable-debug --enable-profile
# OASIS_START
# DO NOT EDIT (digest: d41d8cd98f00b204e9800998ecf8427e)
# OASIS_STOP
init:
	oasis setup -setup-update dynamic
	make
pristine:
	-make distclean
	oasis setup-clean -replace-sections -remove
