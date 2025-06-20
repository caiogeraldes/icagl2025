analysissrc := "analysis/"
falasrc := "fala/"

default:
    just --list

all: analysis fala

analysis:
    cd {{ analysissrc }} && R CMD BATCH main.R
    cd {{ analysissrc }} && R CMD BATCH glm.R

fala:
    cd {{ falasrc }} && lualatex --interaction=batchmode --draftmode main.tex
    cd {{ falasrc }} && biber --quiet main
    cd {{ falasrc }} && lualatex --interaction=batchmode --draftmode main.tex
    cd {{ falasrc }} && lualatex --interaction=batchmode main.tex
