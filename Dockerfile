# get the base image, this one has R, RStudio and pandoc
FROM rocker/verse

# required
MAINTAINER Ben Marwick <benmarwick@gmail.com>

COPY . /mjbnaturepaper
 # go into the repo directory
RUN . /etc/environment \

  # install linux dependency
  && apt-get update \
  && apt-get install gsl-bin  -y \

  # install bioconductor pkg
  && R -e 'install.packages("BiocManager"); BiocManager::install() biocLite("IRanges")' \

  # build this compendium package, get deps from MRAN
  # set date here manually
  && R -e "options(repos='https://mran.microsoft.com/snapshot/2017-07-20'); devtools::install('/mjbnaturepaper', dep=TRUE)" \

 # render the manuscript into a docx
  && R -e "rmarkdown::render('/mjbnaturepaper/analysis/supplementary_information.Rmd')"
