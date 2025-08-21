check:
	R -e "library(devtools); check()"
.PHONY: check

lint:
	R -e "library(styler); style_pkg()"
	R -e "library(roxygen2); roxygenise()"
.PHONY: lint

test:
	R -e "library(devtools); test()"
.PHONY: test
