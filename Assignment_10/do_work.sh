#!/bin/bash
# This is needed to direct the terminal to
# the shell on which the commands will be run.


echo "Running Assignment 10 code"

echo "Running R script"
Rscript Code/code10.R

echo "Finished R script"
echo "Building Latex pdf"

cd Paper

Paper/pdflatex Tex10.tex

cd ..

echo "Done with pdf"
echo "Assignment 10 finished"
