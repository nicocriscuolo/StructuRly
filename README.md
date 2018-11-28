
<!-- README.md is generated from README.Rmd. Please edit that file -->

StructuRly 0.1.0
================

StructuRly is an R package containing a [shiny](https://shiny.rstudio.com) application to produce detailed and interactive graphs of the results of a Bayesian cluster analysis obtained with the most common population genetic softwares used to investigate population structure, such as [STRUCTURE](https://web.stanford.edu/group/pritchardlab/structure.html) or [ADMIXTURE](http://software.genetics.ucla.edu/admixture/), widely used to infer the admixture ancestry of samples starting from the most common genetic markers such as SNPs, AFLPs, RFLPs and microsatellites (such as SSRs). More generally, StructuRly can generate graphs from any file containing admixture information of each sample (encoded in percentages in a range from 0 to 1). The main purpose for which StructuRly was conceived is to provide researchers with detailed graphical outputs to interpret their statistical results through the use of a software with a user-friendly interface, which can therefore be easily used by those who do not know a programming language. In fact, in a typical StructuRly output, the user will have the possibility to display information about the ID of each sample, the original membership assigned by the researcher to the sampled populations (or sub - populations) and the label relative to the sampling site, a variable, the latter, which is used in software for population analysis to support the data analysis algorithm. Furthermore, interactivity is a typical feature of StructuRly outputs, which allows the user to extrapolate even more information through a single chart.

However, StructuRly presents more different features in order to:

-   upload datasets with raw genetic data to analyze them through hierarchical cluster analysis algorithms and view and download the dendrograms based on different distance matrices and linkage methods;

-   produce and customize tables ready to be imported into the STRUCTURE software for the Bayesian analysis (detailed references about the structure of the table to import in STRUCTURE and how to perform its main analysis are available at this [link](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3665925/));

-   import the results of the STRUCTURE analysis into user's pre-formatted tables, in order to insert detailed information about the samples. Likewise, the user can import into StructuRly the results of the population analysis carried out in the ADMIXTURE software (files in **.Q** and **.fam** format), without having to re-format the output in another software (such as R);

-   produce, in addition to the barplot, also the interactive triangle plot, well known graphical STRUCTUER graphical output. Both graphs can show the admixture ancestry of the samples subdivided in a maximum of 20 different clusters;

-   to visually compare the partition obtained from the hierarchical cluster analysis and the one with the Bayesian analysis through a confusion matric and estimate an agreement value with three different agreement indeces.

### Installation

You can install the released version of StructuRly from [GitHub](https://github.com) in RStudio (R version &gt;= 3.5) with:

``` r
library(devtools)

install_github(repo = "nicocriscuolo/StructuRly", dependencies = TRUE)
```

Once the package is loaded and the dependencies installed, you can run the software in the default browser through the following functions:

``` r
library(StructuRly)

runStructuRly()
```

StructuRly 0.1.0 works on all types of browsers (Internet Explorer, Safari, Chrome, etc.) and in its current version it can also work locally and then offline. All you have to do is to install the updated versions of [R](https://www.r-project.org) and [RStudio](https://www.rstudio.com).

### Data input

StructuRly is divided into three different sections depending on the input file chosen. For any type of file, the **header of each variable is mandatory** and varies according to the type of variable that must be present in the input dataset, in particular:

#### Section 1: Import raw genetic data

In this section, the user has the possibility to import his data file in the **.csv** or **.txt** format. This file can contain three optional variables present in the following order and whose header must be exactly the one shown below:

-   **Sample\_ID**: is the variable that contains the IDs of each sample, so each name in this column will be different from the others (although it is good practice to use only numbers and letters, the IDs characters can be separated also by the following symbols "\_" and "-");

-   **Pop\_ID**: is a categorical variable identified by an integer that indicates the population defined by the user for each sample (e.g .: 1, 2, 3 ..);

-   **Loc\_ID**: another categorical variable identified again by an integer that indicates the origin site of each sample; this variable, if present in the table produced with to StructuRly and then imported into STRUCTURE for population analysis can be used by the Bayesian algorithm as a support to the elaboration of the results.

The following variables present in the dataset to import in this section are mandatory, and must contain numerical values relative to the types of markers used. Depending on the ploidy of the organism analyzed, there must be a number of columns for each locus equal to the number of alleles. For the same locus, the column header must contain a single name of the locus followed by the symbol "." and the identification number of the allele, starting from 1 (e.g .: "Locus\_1.1," "Locus\_1.2", etc.). Furthermore, in this type of file the missing values must be indicated only with the abbreviation **NA**.

Below there is an image that represents data stored in a spreadsheet that, once converted to .txt or .csv format, can be properly read by StructuRly:

![image\_1](https://user-images.githubusercontent.com/35098432/49116039-aa40e600-f29c-11e8-82dd-05958633d416.png)

#### Section 2: Import the admixture analysis

Here the user can import a dataset obtained directly following the analysis of his genetic data through a software to investigate population structure. The characteristics of this file are not very different from the one to be imported in the previous section:

-   the three optional variables (**Sample\_ID**, **Pop\_ID** and **Loc\_ID**) can be inserted again, in this precise order, within the import file, with the only difference that in this case the categories of the variables **Pop\_ID** and **Loc\_ID** do not necessarily have to be represented by numbers, but also by characters.

-   the other mandatory variables to be inserted must be those of the admixture proportion calculated by the software mentioned above, and which will be equal in number to the number of clusters chosen by the user before executing the Bayesian analysis. Each of these variables must be identified by a header containing the letter "K" and the number of the relative cluster in sequence (eg: "K1", "K2", "K3", etc.), i. e. in same the order of the dataset exported by the software.

An example of the structure of this type of file is shown below:

![image\_2](https://user-images.githubusercontent.com/35098432/49116054-b88f0200-f29c-11e8-9abf-0c050bdb1e45.png)

-   If your admixture data matrix was obtained as a result of the admixture software, there are two ways to proceed to prepare your StructuRly dataset. From the analysis of a file in .bed or .ped format you will get a **.Q** format file that you can either import into R and then modify as you like, exporting it in .txt or .csv format and then import it into StructuRly (for example, after adding the columns identifying the name of the samples or the population) or you can import directly into StructuRly the **.Q** format file. This file only contains the variables with the values of the ancestry admixture: if you want to add metadata to this dataset you will have to import the **.fam** file into StructuRly, which generally accompanies .bed and .ped files. If you want more details about the information encoded in these types of formats, you can find them [here](https://www.cog-genomics.org/plink2/formats#fam). StructuRly will automatically use the first two variables of the **.fam** file, which are generally used to insure the sample identifier and the user-defined putative population respectively.

Examples of the .txt, .csv, .Q and .fam files that you can import into StructuRly are present at the following link of the repository: [Sample datasets](https://github.com/nicocriscuolo/StructuRly/tree/master/inst/Sample_dataset) (the .Q and the .fam files are obtained after an [ADMIXTURE](http://software.genetics.ucla.edu/admixture/) analysis with the sample files are downloadable directly from the software website).

#### Section 3: Compare partitions

The third section of StructuRly must be used after the files for the first two sections have been loaded and read by StructuRly to compare the partitions obtained from the hierarchical and Bayesian cluster analysis. Obviously the imported datasets must refer to data of same nature and the number of observations must be the same in both files.

### Outputs download

The following image shows the main output of StructuRly (the barplot of the ancestry admixture) downloaded from the application after being customized. The sample labels on the X axis are colored according to the population indicated in the user input file, while the symbols at the top of the plot indicate the sampling site:

![image\_3](https://user-images.githubusercontent.com/35098432/49118209-bbd9bc00-f2a3-11e8-825f-819557ecec66.png)

All StructuRly outputs can be downloaded as images in various high quality formats directly from the application user interface. However, to download the graphs related to the Triangle plot, obtained through a specific function of the *plotly* package (and not with those of *ggplot2*) you need to download the **orca** software in your computer and follow the instructions at this [link](https://github.com/plotly/orca#installation).

### Example

Below is a link to the [YouTube]() video of the application showing an example of using the software:

[![IMAGE ALT TEXT HERE](http://img.youtube.com/vi/0FUFM6GNaYI&feature=youtu.be/0.jpg)](http://www.youtube.com/watch?v=0FUFM6GNaYI&feature=youtu.be)

### Known bugs and issues

-   in the interactive barplot the X axis labels are not colored according to the different populations entered by the user in the input file. To view colored labels, download the image in one of the different formats available. This bug is related to the functions of a third-party package and has been reported to the GitHub community at [link](https://github.com/ropensci/plotly/issues/1328);

-   in the comparison plot the separation lines of the heatmap cells are not visible when using the interactive graph for a bug present in the package used to produce this output. Again, download the output to view the complete chart;

-   the graphic output produced when the observations of the barplot are divided into sections according to the different populations or sampling sites (or both) may not be accurate when using a very large number (&gt; 60) of populations or sites defined a priori by the user. Moreover, in this case, you could see a light overlap between the axis title and the axis text of the barplot.

The slight bugs related to some characteristics of the graphs are shown only in the presence of the interactive plot, but the downloaded file won't present any problem.

### Citation

StructuRly was firstly presented during the [International BBCC meetings](https://www.bbcc-meetings.it) held in Naples in November 2018 and its implementation has been described inside the paper *StructuRly: Elegant, detailed and interactive plots for STRUCTURE and ADMIXTURE population analysis* (submitted). If you use this package for results included in your paper please cite:

    #> 
    #> To cite package 'StructuRly' in publications use:
    #> 
    #>   Nicola Criscuolo and Claudia Angelini (2018). StructuRly:
    #>   Elegant, detailed and interactive plots for STRUCTURE and
    #>   ADMIXTURE outputs. R package version 0.1.0.
    #>   https://github.com/nicocriscuolo/StructuRly
    #> 
    #> A BibTeX entry for LaTeX users is
    #> 
    #>   @Manual{,
    #>     title = {StructuRly: Elegant, detailed and interactive plots for STRUCTURE and ADMIXTURE outputs},
    #>     author = {Nicola Criscuolo and Claudia Angelini},
    #>     year = {2018},
    #>     note = {R package version 0.1.0},
    #>     url = {https://github.com/nicocriscuolo/StructuRly},
    #>   }

### Contact

For additional information regarding StructuRly, please consult the documentation or [email us](mailto:nico.criscuolo981@gmail.com).
