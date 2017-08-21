all: doc test README.md vig

# Render README.Rmd to README.md
README.md: README.Rmd
	Rscript -e "rmarkdown::render('README.Rmd', output_file = 'README.md', output_dir = getwd(), output_format = 'github_document', quiet = TRUE)"
	rm README.html

# Document package (object docs)
doc:
	Rscript -e "devtools::document()"

# Compile vignettes
vig: vignettes/getting-started.Rmd
	Rscript -e "devtools::build_vignettes()"

# Test package
test:
	Rscript -e "devtools::test()"