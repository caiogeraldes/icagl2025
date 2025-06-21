analysissrc := "analysis/"
falasrc := "fala/"
slidessrc := "slides/"
handoutsrc := "handout/"
abstractsrc := "abstract/"
articlesrc := "article/"
releasesrc := "release/"

default:
    just --list

build: all package

all: analysis tex

tex: abstract fala slides article handout

analysis:
    cd {{ analysissrc }} && R CMD BATCH dags.R
    cd {{ analysissrc }} && R CMD BATCH glm.R
    cd {{ analysissrc }} && R CMD BATCH plots_tables.R
    cd {{ analysissrc }} && Rscript -e 'rmarkdown::render("main.Rmd")'

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

package:
    mkdir -p {{ releasesrc }}/analysis
    cp {{ analysissrc }}/model_glm.rds {{ releasesrc }}/analysis/model_glm.rds
    cp {{ analysissrc }}/glm.R {{ releasesrc }}/analysis/glm.R
    cp {{ analysissrc }}/main.html {{ releasesrc }}analysis.html
    cp {{ falasrc }}/main.pdf {{ releasesrc }}/fala.pdf 
    cp {{ slidessrc }}/main.pdf {{ releasesrc }}/slides.pdf 
    cp {{ handoutsrc }}/main.pdf {{ releasesrc }}/handout.pdf 
    # cp {{ articlesrc }}/main.pdf {{ releasesrc }}/article.pdf 
    ouch compress release $(cat version).zip -y
    rm -rdf {{ releasesrc }}
