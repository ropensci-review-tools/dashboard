
all: help

doc: ## Update package documentation with `roxygen2`
	cd ..; \
	Rscript -e "roxygen2::roxygenise('./dashboard')"; \
	cd ./dashboard

check: ## Run `R CMD check` on package
	cd ..; \
	Rscript -e "rcmdcheck::rcmdcheck('./dashboard')"; \
	cd ./dashboard

serve: ## Start local quarto server
	cd quarto; \
	quarto preview

recache: ## Start local quarto server with '--cache-refresh' to force cache refresh
	cd quarto; \
	quarto preview --cache-refresh

dev: serve ## alias for 'serve'

render: ## 'quarto render' command
	cd quarto;	\
	quarto render

renv-update: ## Update 'renv' dependencies to latest versions
	Rscript -e "renv::update()";

renv-snapshot: ## Update the 'renv.lock' file, generally run after `renv-update`
	Rscript -e "renv::snapshot()";

# Lots of variants at:
# https://gist.github.com/prwhite/8168133
# https://stackoverflow.com/questions/35730218/how-to-automatically-generate-a-makefile-help-command

help: ## Show this help
	@printf "Usage:\033[36m make [target]\033[0m\n"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
	
# Phony targets:
.PHONY: render
.PHONY: serve
.PHONY: dev
.PHONY: doc
.PHONY: help
