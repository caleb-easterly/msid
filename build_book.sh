#!/bin/bash

R -e 'library(bookdown); bookdown::render_book("appendix/index.Rmd")'
cd _book
git init
git commit --allow-empty -m 'Update docs'
git checkout -b gh-pages
git add .
git commit -am 'Update docs'
git remote add origin https://github.com/caleb-easterly/msid.git
git push origin gh-pages --force
