all: README.md doc

# Render README.Rmd to README.md
README.md: README.Rmd
	Rscript -e "rmarkdown::render('README.Rmd', output_file = 'README.md', output_dir = getwd(), output_format = 'github_document', quiet = TRUE)"
	Rscript -e "file.remove('README.html')"

# Document package
doc:
	Rscript -e "devtools::document()"

# Clean
clean:
	rm -R README.md