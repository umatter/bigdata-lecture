##################################################
# Shell script to make/update all documents
##################################################



# MATERIALS
# a) render slides (pdf and html)
Rscript code/render_slides.R
# b) clean intermediate results (from interactive session)
rm materials/slides/*.html
rm -r materials/slides/html/*_files
rm -r materials/slides/*_files
rm materials/sourcecode/*Conflict*.R


