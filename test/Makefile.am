AM_ELCFLAGS = -L ${srcdir}/.. -L ${srcdir}/../contrib --eval '(require (quote bytecomp))'
TESTS = indie-org-webmentions-tests \
        indie-org-posse-tests       \
	indie-org-pub-tests         \
	test-state                  \
	indie-org-tests

dist_noinst_LISP = indie-org-webmentions-tests.el \
		   indie-org-posse-tests.el       \
		   indie-org-pub-tests.el         \
		   test-state.el                  \
		   indie-org-tests.el

EXTRA_DIST = $(TESTS) test-state.1 test-state.2 test-state.3

AM_TESTS_ENVIRONMENT =   \
	srcdir=$(srcdir) \
	builddir=$(builddir)

MOSTLYCLEANFILES = test-posts/rss.org test-posts/rss.xml

SUBDIRS = test-posts
