
library_code <- quote({

  library(ade4)
  library(adegenet)
  library(assertthat)
  library(colourpicker)
  library(dendextend)
  library(DT)
  library(grDevices)
  library(janitor)
  library(mcclust)
  library(pegas)
  library(plotly)
  library(poppr)
  library(processx)
  library(randomcoloR)
  library(RColorBrewer)
  library(reshape2)
  library(scales)
  library(shiny)
  library(shinymeta)
  library(shinyjs)
  library(tidyr)

})

eval(library_code)

source("helpers.R")

# fam <- system.file("extdata/sample.fam", package="snpStats")
# bim <- system.file("extdata/sample.bim", package="snpStats")
# bed <- system.file("extdata/sample.bed", package="snpStats")
# sample <- read.plink(bed, bim, fam)
#
# sample$map
# sample$fam
# sample$genotypes[1, 1]
#
# col.summary(sample$genotypes)
