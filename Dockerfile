# start from the rstudio/plumber image
FROM rstudio/plumber

# install the linux libraries needed for plumber
RUN apt-get update -qq && apt-get install -y  libssl-dev  libcurl4-gnutls-dev  libpng-dev pandoc 
    
    
# install plumber, GGally
RUN R -e "install.packages(c('plumber', 'dplyr', 'tidyr', 'ranger', 'recipes', 'parsnip', 'workflows'))"

# copy myAPI.R from the current directory into the container
COPY API.R /app/API.R
COPY /data/NHANES_EnergyDrinks_BP_ages18-29.csv /app/NHANES.csv
COPY Project3_files/figure-html/unnamed-chunk-8-1.png /app/tree_plot.png

WORKDIR /app

# open port to traffic
EXPOSE 8000

# when the container starts, start the myAPI.R script
ENTRYPOINT ["R", "-e", 
    "pr <- plumber::plumb('/app/API.R'); pr$run(host='0.0.0.0', port=8000)"]
