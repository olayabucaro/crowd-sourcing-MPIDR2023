FROM rocker/tidyverse:4.3.1
LABEL maintainer='Name Name'
COPY --chown=${NB_USER} . ${HOME}
USER ${NB_USER}

RUN wget https://github.com/olayabucaro/leaving4life/raw/main/DESCRIPTION && R -e "options(repos = c(binary = 'https://packagemanager.posit.co/cran/__linux__/jammy/2023-07-31', source = 'https://packagemanager.posit.co/cran/2023-07-31')); devtools::install_deps()"

RUN rm DESCRIPTION.1; exit 0
