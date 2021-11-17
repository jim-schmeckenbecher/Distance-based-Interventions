all:	DBI.docx

DBI.docx:	04_Code.R	05_Draft.Rmd
	Rscript	-e	'rmarkdown::render("05_Draft.Rmd")'
04_Code.R: 03_Data.csv
