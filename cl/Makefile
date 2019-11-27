QUICK_LISP ?= $HOME/quicklisp/

gtirb-%: gtirb.asd $(wildcard *.lisp)
	@rm -f $@
	sbcl --noinform --dynamic-space-size 16384 --no-userinit --no-sysinit \
	--load $(QUICK_LISP)/setup.lisp \
	--eval '(ql:quickload :gtirb)' \
	--eval '(ql:quickload :proto-v0)' \
	--eval '(ql:quickload :proto)' \
	--eval '(ql:quickload :gtirb/update)' \
	--eval '(setf uiop/image::*lisp-interaction* nil)' \
	--eval '(asdf:make :gtirb/run-$* :type :program :monolithic t)' \
	--eval '(sb-ext:exit)'
