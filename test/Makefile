###############################################################################
# METIS MAKEFILE
# Copyright (c) 2001 Joe Leslie-Hurd, distributed under the MIT license
###############################################################################

IGNORE = '^User: \|random sampling'

###############################################################################
# The default action
###############################################################################

.PHONY: default
default: mosml

###############################################################################
# Cleaning up temporary files
###############################################################################

TMPFILE1 = /tmp/test-make-file1
TMPFILE2 = /tmp/test-make-file2

TEMP = $(TMPFILE1) $(TMPFILE2) mosml-result mlton-result polyml-result

.PHONY: clean
clean:
	rm -f $(TEMP)

###############################################################################
# The Moscow ML self-test
###############################################################################

MOSML = mosml -quietdec -I ../bin/mosml

.PHONY: mosml
mosml: mosml-result mosml-diff

.PHONY: diff
mosml-diff:
	@grep -v $(IGNORE) <result.ok >$(TMPFILE1)
	@grep -v $(IGNORE) <mosml-result >$(TMPFILE2)
	@diff -c $(TMPFILE1) $(TMPFILE2)
	@rm -f $(TMPFILE1) $(TMPFILE2)

mosml-result: test.sml ../src/problems.sml ../bin/mosml/Options.uo
	rm -f $@
	echo "quit();" | $(MOSML) $< 2>&1 | tee $@

###############################################################################
# The MLton self-test
###############################################################################

.PHONY: mlton
mlton: mlton-result mlton-diff

.PHONY: mlton-diff
mlton-diff:
	@grep -v $(IGNORE) <result.ok >$(TMPFILE1)
	@grep -v $(IGNORE) <mlton-result >$(TMPFILE2)
	@diff -c $(TMPFILE1) $(TMPFILE2)
	@rm -f $(TMPFILE1) $(TMPFILE2)

mlton-result: ../bin/mlton/selftest
	rm -f $@
	$< 2>&1 | tee $@

###############################################################################
# The Poly/ML self-test
###############################################################################

.PHONY: polyml
polyml: polyml-result polyml-diff

.PHONY: polyml-diff
polyml-diff:
	@grep -v $(IGNORE) <result.ok >$(TMPFILE1)
	@grep -v $(IGNORE) <polyml-result >$(TMPFILE2)
	@diff -c $(TMPFILE1) $(TMPFILE2)
	@rm -f $(TMPFILE1) $(TMPFILE2)

polyml-result: ../bin/polyml/selftest
	rm -f $@
	$< 2>&1 | tee $@
