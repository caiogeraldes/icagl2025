analysissrc := "analysis/"
falasrc := "fala/"
slidessrc := "slides/"
handoutsrc := "handout/"
abstractsrc := "abstract/"
articlesrc := "article/"

default:
    just --list

all: analysis tex

tex: abstract fala slides article handout

analysis:
    cd {{ analysissrc }} && R CMD BATCH main.R
    cd {{ analysissrc }} && R CMD BATCH glm.R

fala:
    cd {{ falasrc }} && lualatex --interaction=batchmode --draftmode main.tex
    cd {{ falasrc }} && biber --quiet main
    cd {{ falasrc }} && lualatex --interaction=batchmode --draftmode main.tex
    cd {{ falasrc }} && lualatex --interaction=batchmode main.tex

slides:
    cd {{ slidessrc }} && lualatex --interaction=batchmode --draftmode main.tex
    cd {{ slidessrc }} && biber --quiet main
    cd {{ slidessrc }} && lualatex --interaction=batchmode --draftmode main.tex
    cd {{ slidessrc }} && lualatex --interaction=batchmode main.tex

article:
    cd {{ articlesrc }} && lualatex --interaction=batchmode --draftmode main.tex
    cd {{ articlesrc }} && biber --quiet main
    cd {{ articlesrc }} && lualatex --interaction=batchmode --draftmode main.tex
    cd {{ articlesrc }} && lualatex --interaction=batchmode main.tex

abstract:
    cd {{ abstractsrc }} && lualatex --interaction=batchmode --draftmode main.tex
    cd {{ abstractsrc }} && biber --quiet main
    cd {{ abstractsrc }} && lualatex --interaction=batchmode --draftmode main.tex
    cd {{ abstractsrc }} && lualatex --interaction=batchmode main.tex

handout:
    cd {{ handoutsrc }} && lualatex --interaction=batchmode --draftmode main.tex
    cd {{ handoutsrc }} && biber --quiet main
    cd {{ handoutsrc }} && lualatex --interaction=batchmode --draftmode main.tex
    cd {{ handoutsrc }} && lualatex --interaction=batchmode main.tex
