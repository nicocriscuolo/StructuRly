
<!-- README.md is generated from README.Rmd. Please edit that file -->

# StructuRly 0.1.0

**StructuRly** is an **R** package containing a
[shiny](https://shiny.rstudio.com) application to produce detailed and
interactive graphs of the results of a Bayesian cluster analysis
obtained with the most common population genetic software used to
investigate population structure, such as *STRUCTURE* or *ADMIXTURE*.
These software are widely used to infer the admixture ancestry of
samples starting from genetic markers such as SNPs, AFLPs, RFLPs and
microsatellites (such as SSRs). More generally, **StructuRly** can
generate graphs from any file containing admixture information of each
sample (encoded in percentages in a range from 0 to 1). We developed
**StructuRly** to provide researchers with detailed graphical outputs to
interpret their statistical results through the use of software with a
user-friendly interface, which can, therefore, be easily used by those
who do not know a programming language. In fact, in a typical
**StructuRly** output, the user will have the possibility to display
information about the ID of each sample, the original membership
assigned by the researcher to the sampled populations (or
subpopulations) and the label of the sampling site, a variable, the
latter, which is used in software for population analysis to support the
data analysis algorithm. Furthermore, interactivity is a typical feature
of **StructuRly** outputs, which allows the user to extrapolate even
more information through a single chart.

However, this shiny application presents more different features to:

  - support the statistical genetic analysis with necessary information
    about the molecular markers and diversity indices and through the
    calculation of the \(P_{gen}\) (if you have haploid or diploid data)
    or the Hardy-Weinberg equilibrium for every locus. For the
    calculation of the Hardy-Weinberg equilibrium, the \(p\)-value of
    the \(\chi^2\)-test can be calculated for any level of ploidy (\>=
    2), while the exact \(p\)-value from the Monte Carlo test is
    currently available just for diploids (more details are available
    inside the
    [pegas](https://cran.r-project.org/web/packages/pegas/pegas.pdf)
    package manual);

  - upload datasets with raw genetic data to analyze them through the
    principal coordinates analysis (MDS) and hierarchical cluster
    analysis algorithms, and view and download the dendrograms based on
    different distance matrices and linkage methods;

  - produce and customize tables ready to be imported into the
    *STRUCTURE* software for the Bayesian analysis;

  - import the results of the *STRUCTURE* and *ADMIXTURE* population
    analysis directly into **StructuRly** in different formats, without
    having to re-structure the dataset with other software (such as
    **R**);

  - produce an interactive barplot and triangle plot, the most
    well-known *STRUCTURE* graphical outputs. Both graphs can show the
    admixture ancestry of the samples subdivided in a maximum of 20
    different clusters;

  - visually compare the partition obtained from the hierarchical
    cluster analysis and the one from the Bayesian (*STRUCTURE*) or
    maximum likelihood (*ADMIXTURE*) analysis through a confusion matrix
    and estimate an agreement value of the two partitions with two
    different agreement indices.

  - visualize and download the *R* code used inside the shiny
    application to produce all the plots.

### Installation

You can install the released version of **StructuRly** from
[GitHub](https://github.com) in *RStudio* with:

``` r
install.packages(pkgs = "devtools")

library(devtools)

install_github(repo = "nicocriscuolo/StructuRly", dependencies = TRUE)
```

Once the package is loaded and the dependencies installed, you can run
the software in the default browser through the following functions:

``` r
library(StructuRly)

runStructuRly()
```

If you have trouble installing **StructuRly** you can follow the
instructions present this
[link](https://github.com/nicocriscuolo/StructuRly/blob/master/inst/Instructions_install_StructuRly.txt).

### System requirements

**StructuRly** works on macOS, Windows and Linux operative systems.
Install the updated version of [R](https://www.r-project.org) (\>= 3.5)
and [RStudio](https://www.rstudio.com) and launch **StructuRly** on all
types of browsers (Internet Explorer, Safari, Chrome, etc.). In its
current version, it can also work locally and then offline. If you need
any information about the usage of *STRUCTURE* or *ADMIXTURE* software
(e. g. instructions to launch the software, preparation of input files
and how to exports the outputs), please visit their websites at the
following links:

  - [STRUCTURE](https://web.stanford.edu/group/pritchardlab/structure.html)

  - [ADMIXTURE](http://software.genetics.ucla.edu/admixture/)

Moreover, the user can launch the Terminal (to start an *ADMIXTURE*
population analysis) or the *STRUCTURE* software directly from the user
interface of **StructurRly** (this function is currently available for
macOS and Linux users). To make this buttons work, both software must be
installed on your computer.

**N. B.**: If you use a Linux based machine, to properly configure **R**
and to install some **StructuRly** dependencies you may need specific
Linux libraries to make these software work with this operative system.
To install these libraries in **R** follow the instructions displayed
inside the **R** console when you load the dependency packages.

### Online version

If you are not familiar with *R* or *RStudio* you can access to
**StructuRly** directly from the web by using the following link:
<https://nicocriscuolo.shinyapps.io/StructuRly/>.

### Data input

**StructuRly** is divided into three different sections depending on the
input file chosen. For any type of file, the **header of each variable
is mandatory** and varies according to the type of variable that must be
present in the input dataset. When you start a new session of
**StructuRly**, if you change the uploaded file with a new one (inside
the same section), to produce new outputs remember to re-define every
time the **type of separator** (e. g. column, semi-column or tab) and to
indicate if your data have **quotation marks**.

###### Data format

In the first section of **StructuRly**, you can import both **.txt** and
**.csv** file. Since the second section also accepts the output file
obtained after the population analysis performed with *ADMIXTURE*, here
you can import also **.Q** format file and a **.fam** file (if the
latter one is available).

In **StructuRly** you also have the possibility to export a table ready
to be imported inside the *STRUCTURE* software. If you need detailed
references about the structure of this dataset and how to perform the
population analysis with *STUCTURE* you can find them this
[link](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3665925/). If you
want to use your raw genetic data to produce an input table for the
*ADMIXTURE* software, you have to convert your matrix in a **.ped** or
**.bed** file. You can do that through the functionalities of the
*PLINK* software, illustrated step by step at this
[link](http://zzz.bwh.harvard.edu/plink/data.shtml). If you need more
information about this last data formats, they are available
[here](https://www.cog-genomics.org/plink2/formats#bed).

###### Download sample datasets

Examples of the **.txt**, **.csv**, **.Q** and **.fam** files that you
can import into **StructuRly** are present at the following repository
link: [Sample
datasets](https://github.com/nicocriscuolo/StructuRly/tree/master/inst/Sample_dataset)
(the .Q and the .fam files are obtained after an *ADMIXTURE* analysis
with the sample files downloadable directly from the
[ADMIXTURE](http://software.genetics.ucla.edu/admixture/) website).  
To download the sample datasets from GitHub, right-click on the desired
file and choose **Download linked file**. The sample datasets are
available in pair of two files: one contains the raw genetic data and
the other the results of the *STRUCTURE* analysis performed on such
data. They have different format and information to describe different
use-case scenario, in particular:

  - **Sample1**: this datasets in **.txt** format contains random
    generated values of genetic triploid loci (with different names) in
    500 samples, with a weight that ranges from 150 to 500 base-pairs.
    The additional information available are the Sample ID, the
    Population ID and Location ID (see Section 1);
  - **Sample2**: this datasets in **.csv** format contain information
    related to diploid genetic loci of simple sequence repeats (SSR)
    sampled in 95 *Olea europaea* specimens in [Criscuolo et
    al., 2019](https://www.mdpi.com/2223-7747/8/9/297). They contain
    additional information about the Sample ID and the Population ID;
  - **Sample3**: the last sample dataset is in **.Q** format and
    contains the results of the *ADMIXTURE* analysis on a genetic
    dataset available on the *ADMIXTURE* website. Moreover, the **.fam**
    file is available to add the Sample ID and the Population ID to the
    original dataset.

#### Section 1: Import raw genetic data

The input for this section can contain three optional variables present
in the following order and whose header must be precisely the one shown
below:

  - **Sample\_ID**: is the variable that contains the IDs of each sample
    so each name in this column will be different from the others
    (although it is good practice to use only numbers and letters, the
    IDs characters can also be separated by the following symbols: "\_"
    and “-”);

  - **Pop\_ID**: is a categorical variable identified by an integer that
    indicates the putative population defined by the user for each
    sample (e.g .: 1, 2, 3, etc.);

  - **Loc\_ID**: another categorical variable identified again by an
    integer that indicates the origin site of each sample; this
    variable, if present in the table produced with **StructuRly** and
    then imported into *STRUCTURE* for population analysis, can be used
    by the Bayesian algorithm as support to results elaboration.

The following variables present in the dataset to import in this section
are mandatory and must contain numerical values relative to the types of
markers used. Depending on the ploidy of the organism analyzed, there
must be a number of columns for each locus equal to the number of
alleles, in particular:

  - for haploid organisms, each column must have a unique name of a
    locus coded with alpha-numeric characters, but it must not contain
    the dot symbol (e. g.: “Locus\_1”, “Locus\_2”, “UDO”, “BRAC8792”,
    etc.);

  - for diploid and polyploid organisms the column header must contain a
    single name of the locus followed, this time, by the dot symbol
    (“.”) and the identification number of the allele, mandatorily
    starting from 1 (e.g .: “Locus\_1.1,” “Locus\_1.2”, “UDO.1”,
    “UDO.2”, etc.). Below there is an image that represents data
    stored in a spreadsheet that, once converted in **.txt** or **.csv**
    format, can be appropriately read by **StructuRly**:

![image\_1](https://user-images.githubusercontent.com/35098432/49116039-aa40e600-f29c-11e8-82dd-05958633d416.png)

**N. B.**: for the **Sample\_ID**, **Pop\_ID** and **Loc\_ID** columns,
avoid the usage of the name “NA” to indicate a name of a sample, of a
putative population or a collection site, because **StructuRly** could
recognize that characters as a missing value and the plot will not
display the correct information. This also applies for the preparation
of the input datasets for the Section 2.

###### Missing values

When you produce the file for this section of **StructuRly**, the
missing values must be indicated only with the abbreviation **NA**. The
cells of the reactive table (in the table panel named “Input table”)
that contain missing values will appear empty, while they are codified
as **-9** in the table that can be produced and downloaded by
**StructuRly** to be imported into *STRUCTURE*.

**N. B.**: if your data refer to diploid or polyploid organisms and you
encounter a missing value in one or more of your samples in a specific
locus, the **NA** value must be present for all the alleles of that
locus;

#### Section 2: Import population analysis

Here the user can import a dataset obtained directly following the
population analysis of his genetic data. The characteristics of this
input file are not very different from the one to be imported in the
previous section:

  - the three optional variables (**Sample\_ID**, **Pop\_ID** and
    **Loc\_ID**) can be inserted again, in this precise order, within
    the import file, with the only difference that in this case the
    categories of the variables **Pop\_ID** and **Loc\_ID** do not
    necessarily have to be represented by numbers, but also by
    characters.

  - the other mandatory variables to be inserted must be those of the
    admixture proportion calculated by the population software mentioned
    above, and which will be equal in number to the number of clusters
    chosen by the user before executing the Bayesian analysis. Each of
    these variables must be identified by a header containing the letter
    “K” and the number of the relative cluster in sequence (e. g.: “K1”,
    “K2”, “K3”, etc.), i. e. in the same order of the dataset exported
    by the software.

Below there is an example of this type of file structure. In this case
the **Loc\_ID** column is not present; in fact, the three information
variable are not mandatory for the datasets to import in section 1. and
2.:

![image\_2](https://user-images.githubusercontent.com/35098432/49116054-b88f0200-f29c-11e8-9abf-0c050bdb1e45.png)

  - If you have obtained the results of your population analysis with
    the *ADMIXTURE* software, there are two ways to proceed to prepare
    the dataset for **StructuRly**. From the analysis of a file in
    **.bed** or **.ped** format you will get a **.Q** format file that
    you can either import into **R** and then modify as you like,
    exporting it in **.txt** or **.csv** format and then import it into
    **StructuRly** (for example, after adding the columns identifying
    the name of the samples or the population) or you can import
    directly into **StructuRly** the **.Q** format file. This file only
    contains the variables with the values of the ancestry admixture: if
    you want to add metadata to this dataset you will have to import the
    **.fam** file into **StructuRly**, which generally accompanies
    **.bed** and **.ped** files. **StructuRly** will automatically use
    the first two variables of the **.fam** file, which are generally
    used to ensure the sample identifier and the user-defined population
    respectively.

#### Section 3: Compare partitions

The third section uses the first two sections input files to compare the
partitions obtained from the hierarchical and Bayesian cluster analysis.
Obviously, the imported datasets must refer to data of same nature and
the number of observations must be the same in both files. The samples
cluster memberships of the admixture ancestry analysis partition are
assigned considering the highest value of ancestry found in a specific
population (*STRUCTURE* or *ADMIXTURE* cluster) for each sample. It
means that this partition will divide the observations in the same
number of clusters chosen for the population analysis, but if the
admixture ancestry is the lowest for a particular subpopulation, this
cluster will not be shown in the comparison plot and table, because
there are no observations assigned to it.

### Outputs download

The following image shows the main output downloaded from
**StructuRly**, the barplot of the ancestry admixture. The sample labels
on the X axis are colored according to the population indicated in the
user input file, while the symbols at the top of the plot indicate the
sampling site. In **StructuRly** there are 25 different symbols
available but you can also simply decide to split the entire plot on the
basis of the different categories inside the **Pop\_ID** and **Loc\_ID**
variables.

![image\_3](https://user-images.githubusercontent.com/35098432/52059268-b16cc480-2569-11e9-87c9-f6c1bbaa3962.png)

All **StructuRly** outputs can be downloaded as images in various
high-quality formats directly from the user interface. However, to
download the graphs related to the Triangle plot, obtained through a
specific function of the *plotly* package (and not with those of
*ggplot2*) you need to download the **orca** software in your computer
and follow the instructions at this
[link](https://github.com/plotly/orca#installation). If you don’t
install the **orca** software you can always download the Triangle plot
through the functionalities of the *plotly* package through the commands
displayed directly on the interactive plot.

**N. B.**: for a dataset with a high sample number (\> 500) remember to
re-size your plot (width, height and resolution) to better distinguish
the bars and the relative IDs.

### Example

Here’s a link to the [YouTube video of
StructuRly](https://youtu.be/0FUFM6GNaYI) showing an example of using of
the software. Moreover, the flowchart below, accessible from the
**Instructions** panel of the application, schematize a tutorial to use
the software.

![image\_4](https://user-images.githubusercontent.com/35098432/73140059-05bf5d00-4075-11ea-867f-e59cd4a9724f.png)

### Known bugs and limitations

  - in the interactive barplot, the X-axis labels are not colored
    according to the different populations entered by the user in the
    input file. To view colored labels, download the image in one of the
    different formats available. This bug is related to the functions of
    a third-party package and has been reported to the GitHub community
    at this [link](https://github.com/ropensci/plotly/issues/1328);

  - there is a limit of 50 Mb for data uploading;

  - for more than 40 predefined populations present in the dataset to
    produce the barplot, the colors used to distinguish such populations
    within the labels of the X-axis of the barplot could start to
    repeat;

  - in the comparison plot, the separation lines of the heatmap cells
    are not visible when using the interactive graph for a bug present
    in the package used to produce this output. Again, download the
    output to view the complete chart;

  - when using a large number of populations (\> 60) or collection sites
    defined a priori by the user, the graphics output produced when the
    observations of the barplot are divided into sections according to
    the different populations or collection sites (or both) may not be
    accurate. Moreover, in this case, you could see a slight overlap
    between the axis title and the axis text of the barplot;

  - when using the online version of StructuRly, in order to download
    the code to produce a plot click on “Download R Code” and then
    select the code in the dashboard, then copy it (Cmd + C on macOS,
    CTRL + C on Windows and Ubuntu). Then button to directly copy some
    content to the user clipboard in an interactive session is still in
    development.

The slight bugs related to some characteristics of the graphs are shown
only inside the interactive plots, but the downloaded file won’t present
any problem.

### Citation

**StructuRly** was firstly presented during the [International BBCC
meetings](https://www.bbcc-meetings.it) held in Naples in November 2018
and its implementation has been described inside the paper *StructuRly:
a novel shiny app to produce comprehensive, detailed and interactive
plots for population genetic analysis* (submitted). If you use this
package for your research please cite:

  - Criscuolo, N. G. & Angelini, C. StructuRly: A novel shiny app to
    produce comprehensive, detailed and interactive plots for population
    genetic analysis. PLoS One 15, e0229330 (2020).
    <https://doi.org/10.1371/journal.pone.0229330>.

### Contact

For additional information regarding **StructuRly**, please consult the
documentation or [email us](mailto:nico.criscuolo981@gmail.com).
