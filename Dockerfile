# get the base image, this one has R, RStudio and pandoc
FROM rocker/verse:3.3.2

# required
MAINTAINER Ben Marwick <benmarwick@gmail.com>

COPY . /mjbnaturepaper
 # go into the repo directory
RUN . /etc/environment \
  
  # install linux dependency 
  && apt-get update \
  && apt-get install libgsl0-dev  -y \
  
  # build this compendium package, get deps from MRAN
  # set date here manually
  && R -e "options(repos='https://mran.microsoft.com/snapshot/2017-07-20'); devtools::install('/mjbnaturepaper', dep=TRUE)" \

 # render the manuscript into a docx
  && R -e "rmarkdown::render('/mjbnaturepaper/analysis/supplementary_information.Rmd)"
