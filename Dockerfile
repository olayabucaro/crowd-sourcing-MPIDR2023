FROM rocker/tidyverse:4.2.3
LABEL maintainer='Name Name'

ENV NB_USER=rstudio
USER ${NB_USER}
WORKDIR /home/${NB_USER}
COPY --chown=${NB_USER} . ${HOME}

RUN R -e "install.packages( c( \
    'lme4', \
    'countrycode', \
    'maps', \
    'stargazer', \
    'margins', \
    'marginaleffects', \
    'ggpubr'), \
   dependencies=TRUE,  \
   repos=c(binary = 'https://packagemanager.posit.co/cran/__linux__/jammy/2023-07-31', source = 'https://packagemanager.posit.co/cran/2023-07-31'))"
   
