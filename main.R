####################################################
# REPORT SCRIPT
# AUTHOR Yanpeng Lin
####################################################

options(warn = 2, error = recover)
# library load
libs = c("rmarkdown")
lapply(libs, library, character.only=TRUE)

# generating report
render("doc/report.Rmd", html_document())
render("doc/report.Rmd", pdf_document())
