
doc:
	cd ..; \
	Rscript -e "roxygen2::roxygenise('./dashboard')"; \
	cd ./dashboard

serve: ## Start local quarto server
	cd quarto; \
	quarto preview

build: ## readthedocs 'make html' command
	cd quarto;	\
	quarto build

# Lots of variants at:
# https://gist.github.com/prwhite/8168133
# https://stackoverflow.com/questions/35730218/how-to-automatically-generate-a-makefile-help-command

help: ## Show this help
		@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
	
# Phony targets:
.PHONY: build
.PHONY: serve
.PHONY: doc
.PHONY: help
