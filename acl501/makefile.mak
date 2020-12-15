clean:
	cd .. ; find . -name "*.fasl" -print | xargs rm -f
	cd .. ; find . -name ".#*" -print | xargs rm -f
	rm -f autoloads.out

tags:
	cd .. ; rm -f TAGS
	cd .. ; find acl501 -name "*.lisp" -print | xargs etags -a
	cd .. ; find server -name "*.lisp" -print | xargs etags -a
	cd .. ; find client -name "*.lisp" -print | xargs etags -a
	cd .. ; find smtp -name "*.lisp" -print | xargs etags -a
