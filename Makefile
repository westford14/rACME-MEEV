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

build-shiny:
	docker build --platform linux/amd64 -t r-acme-shiny:latest -f shiny/Dockerfile .
.PHONY: build-shiny

run-shiny: build-shiny
	docker run --platform linux/amd64 -p 8180:8180 -it r-acme-shiny:latest
.PHONY: run-shiny
