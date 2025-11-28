FROM rocker/shiny-verse:4.5.1

RUN sudo apt-get update -y && sudo apt-get install build-essential jags -y

RUN mkdir /shiny-app

COPY . /shiny-app

RUN R -e "library('devtools'); setwd('/shiny-app/'); devtools::install(); install.packages(c('caret', 'glmnet'))"

EXPOSE 8180

CMD Rscript -e "setwd('/shiny-app/'); library('shiny'); runApp('shiny/', host = '0.0.0.0', port = 8180)"