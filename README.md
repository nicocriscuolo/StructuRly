
<!-- README.md is generated from README.Rmd. Please edit that file -->
StructureIt 0.1.0
=================

OliveR is a R package containing a [shiny](https://shiny.rstudio.com) application that provides a wide series of statistical tools for exploring and analyzing both quantitative (such as morphometric or biochemical) and genetic data that can be associated to geographical coordinates. Through the use of shapefiles, the application allows to visualize both data and results of a statistical analysis directly on a geographical basis.

It is a general-purpose interactive application that make available several functions for explorative data analysis as well as several standard methods for multivariate statistics (such as Linear models, Principal Component Analysis, Cluster analysis, etc.) and therefore guides the user in visualizing, exploring and analyzing the data by a simple point-and-click approach.

In brief, OliveR contains the following main functionalities:

-   Interactive explorative graphics, with scatter plots, barplots, boxplots and geographical plots;
-   One way ANOVA (and Bonferroni post hoc test);
-   Principal Component Analysis;
-   Cluster analysis (hierarchical clustering, K-means, PAM, and methods for assessing cluster quality such as silhouette coefficient and adjusted rand index);
-   Mantel test.

Installation
------------

You can install the released version of OliveR from [GitHub](https://CRAN.R-project.org) with:

``` r
library(devtools)

install_github(repo = "nicocriscuolo/StructureIt")
```

Once the package is loaded and the dependencies installed, you can run the software in the default browser through the following functions:

``` r
library(OliveR)

runOliveR()
```

OliveR 0.1.0 works on all types of browsers (Internet Explorer, Safari, Chrome, etc.) and in its current version, if you do not use the features of Google Maps, it can also work locally and then offline. All you have to do is to install the updated versions of [R](https://www.r-project.org) and [RStudio](https://www.rstudio.com).

Data input
----------

In this first version, in order for the software to be used, you need to import two types of files:

-   a user-defined **.csv file**: the first four columns must contain the statistical variables that indicate, in order, the identifier of the sample, the group to which it belongs, and the UTM East and North coordinates; therefore the names of these 4 columns are: "Sample\_ID", "Label", "UTM\_Est", "UTM\_Nord". The following columns are dynamic and will contain the variables on which the user wants to perform the analyzes. These columns may contain either data of a continuous nature or data of the weight in bp (base pairs) of microsatellite loci (SSRs): in the latter case each column is defined by the name of the locus and must contain the values of the two alleles (diploid organism) divided by the symbol "/". Examples of the .csv files that you can import into OliveR is present at the following link of the repository: [CSV data](https://github.com/nicocriscuolo/OliveR/tree/master/inst/CSV_data). Depending on the type of .csv file that will be imported, you will be able to take advantage of different software features.

-   a **shapefile** (**.shp**) on which to display the spatial arrangement of the samples. The shapefile must be inside a folder (local path on the user's computer) in which there are also all the accessory files that are generally associated with a shapefile and that allow its operation (as in the GIS softwares) such as .cpg, .dbf, .prj, .sbn, .sbx, .shx. If you want to downlad an example folder with geo-spatial files related to the .csv data present in this repository, you can do it at this link: [shapefile data](https://github.com/nicocriscuolo/OliveR/tree/master/inst/shpefile_data).

Example
-------

Below is a link to the [YouTube](https://www.youtube.com/?gl=IT&hl=it) video of the application showing an example of using the software:

[![IMAGE ALT TEXT HERE](http://img.youtube.com/vi/WdUzk_eZjXU/0.jpg)](http://www.youtube.com/watch?v=WdUzk_eZjXU)

If you have a Google API to use the maps, you can view the disposition of your samples, and the information associated with it, including through the [Google Maps](https://www.google.com/maps) software. You can generate your API key at any time to access the software within the [Google Cloud Platform](https://cloud.google.com) and display the map directly on OliveR.

![oliver\_maps\_1](https://user-images.githubusercontent.com/35098432/44960136-3049cc80-aefa-11e8-9bf8-b3641b1d6e04.jpg)

Citation
--------

OliveR was designed to analyze data of a continuous and genetic (SSRs) nature obtained from samples that are spatially distributed in a given geographical area, in order to quantify the differences in morphometric, biochemical and genetic parameters. Its first use, from which the name derives, was made on olive trees present in southern of Italy, and the results have been included inside the paper [article title](https://) submitted to the journal [Food Chemistry](https://www.journals.elsevier.com/food-chemistry).

Contact
-------

For additional information regarding OliveR, please consult the documentation or [email us](mailto:nico.criscuolo981@gmail.com).

Inserire qui il link alla pagina shinyapp.io
============================================
