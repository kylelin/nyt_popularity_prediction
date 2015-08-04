####################################################
# REPORT SCRIPT
# AUTHOR Yanpeng Lin
####################################################

# library load
libs = c("rmarkdown")
lapply(libs, library, character.only=TRUE)

# generating report
render("doc/report.Rmd")
