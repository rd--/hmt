#!/bin/bash

mkdir -p svg
rm -f svg/*.svg
for i in *.dot ; do echo $i ; dot -Tsvg $i > ${i%.dot}.svg ; done
mv *.svg svg
