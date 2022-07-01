dist_lisp_LISP = indie-org.el indie-org-pkg.el
EXTRA_DIST = README.org
CLEANFILES = .pkg-tmp/ indie-org-$(PACKAGE_VERSION).tar
SUBDIRS = contrib test
AM_ELCFLAGS = -L contrib

package: indie-org-$(PACKAGE_VERSION).tar $(srcdir)/README.org

srclisp=$(dist_lisp_LISP:%.el=$(srcdir)/%.el)

indie-org-$(PACKAGE_VERSION).tar: $(srclisp) $(srcdir)/README.org
	mkdir -p .pkg-tmp/indie-org-$(PACKAGE_VERSION)/ && \
	cp $(srclisp) .pkg-tmp/indie-org-$(PACKAGE_VERSION)/ && \
	cp $(srcdir)/README.org .pkg-tmp/indie-org-$(PACKAGE_VERSION)/ && \
	cd .pkg-tmp && tar cf $@ indie-org-$(PACKAGE_VERSION)/ && \
	cd .. && mv -v .pkg-tmp/indie-org-$(PACKAGE_VERSION).tar . && \
	rm -rf .pkg-tmp

dist-hook: package
