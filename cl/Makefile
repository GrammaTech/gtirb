QUICK_LISP ?= ${HOME}/quicklisp/

all: gtirb-dot gtirb-update doc

gtirb-%: gtirb.asd $(wildcard *.lisp)
	@rm -f $@
	sbcl --noinform --dynamic-space-size 16384 --no-userinit --no-sysinit \
	--load $(QUICK_LISP)/setup.lisp \
	--eval '(ql:quickload :gtirb/$*)' \
	--eval '(setf uiop/image::*lisp-interaction* nil)' \
	--eval '(asdf:make :gtirb/run-$* :type :program :monolithic t)' \
	--eval '(sb-ext:exit)'

doc: gtirb-cl-api.html

gtirb-cl-api.html: README.md gtirb.asd $(wildcard *.lisp)
	./write-documentation gtirb $< $@
