AM_ELCFLAGS = -L ${srcdir}/.. -L ${srcdir}/../contrib

TESTS = indie-org-tests test-pub-state

dist_noinst_LISP = indie-org-tests.el test-pub-state.el

EXTRA_DIST = $(TESTS)

AM_TESTS_ENVIRONMENT =   \
	srcdir=$(srcdir) \
	builddir=$(builddir)

MOSTLYCLEANFILES = test-posts/rss.org test-posts/rss.xml

SUBDIRS = test-posts
