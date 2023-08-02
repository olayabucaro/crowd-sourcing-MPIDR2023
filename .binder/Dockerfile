FROM rocker/binder:4.3.1
LABEL maintainer='Orlando Olaya Bucaro'
COPY --chown=${NB_USER} . ${HOME}
USER ${NB_USER}



RUN wget https://github.com/olayabucaro/crowd-sourcing-MPIDR2023/raw/master/DESCRIPTION && R -e "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2023-08-02/')); devtools::install_deps()"

RUN rm DESCRIPTION.1; exit 0
