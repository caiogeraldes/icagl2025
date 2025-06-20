analysissrc := "analysis/"
falasrc := "fala/"
slidessrc := "slides/"
articlesrc := "article/"

default:
    just --list

all: analysis fala slides

tex: fala slides article

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
