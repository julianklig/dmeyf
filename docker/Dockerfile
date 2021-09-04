# Use r-base image from rocker
FROM rocker/r-base:latest

# Create dmeyf directories
RUN mkdir -p ~/datasets
RUN mkdir -p ~/datasetsOri
RUN mkdir -p ~/kaggle
RUN mkdir -p ~/work
RUN mkdir -p ~/TareasHogar

# Install system packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libudunits2-dev \
    libgdal-dev \
    libcurl4-openssl-dev \
    libssl-dev

# Use script to handle install.packages failures
ADD install_R_package.R /

# Install R packages
# misc
RUN Rscript --no-save install_R_package.R \
    Matrix \
    Hmisc \
    rlist \
    yaml \
    primes \
    bit64 \
    IRdisplay \
    repr \
    vioplot \
    DT \
    ROCR \
    R.utils

# low level
RUN Rscript --no-save install_R_package.R \
    Rcpp \
    devtools

# graphs
RUN Rscript --no-save install_R_package.R \
    ggplot2 \
    gganimate \
    transformr \
    DiagrammeR

# Dataset management
RUN Rscript --no-save install_R_package.R \
    data.table

# Decission trees
RUN Rscript --no-save install_R_package.R \
    rpart \
    rpart.plot \
    treeClust

# Bagging
RUN Rscript --no-save install_R_package.R \
    ranger \
    randomForest

# Boosting
RUN Rscript --no-save install_R_package.R \
    xgboost \
    lightgbm

# Bayesian Optimization
RUN Rscript --no-save install_R_package.R \
    DiceKriging \
    mlrMBO

CMD ["R"]
