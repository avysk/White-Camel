CONFIGUREFLAGS = --enable-ui --enable-tests --disable-debug --enable-profile
# OASIS_START
# OASIS_STOP
init:
	oasis setup -setup-update dynamic
	make
pristine:
	-make distclean
	rm -f gmon.out
	oasis setup-clean -replace-sections -remove
