
# GeneApp: Interactively Explore Genetic Data <img src = "inst/shinyapps/shiny_geneApp/www/img/logo1.jpg" align = "right" style = "width: 115px; height: 110px;"/>

## Overview

You are visiting the Github repository of the GeneApp package.  

GeneApp is a browser-based interface for interactive exploratory
analysis of genomic data in [R](https://www.r-project.org/). The
application is based on the [Shiny](https://shiny.rstudio.com/) package
and can be run locally or on a server.

The ultimate goal of the project is to provide an intuitive interface,
with a comprehensive set of tools, that will help researchers to import,
explore and visualize large cancer genomics studies.  
Users also have the opportunity to download summaries and reports in
various formats to best reflect their analysis.  
Due to the heterogeneous nature of genomic data there are some
restrictions on the formatting of the files and data sets that the app
can handle, all the details are listed in the [Advanced
Usage](#advanced-usage) section.  

Please use the issue tracker on GitHub to suggest enhancements or report
problems: <https://github.com/FilippoMazzara/Progetto_IFO/issues>. For
other questions and comments please contact the author Filippo Mazzara.

GeneApp has two main purposes:  
- Provide a fast way to researchers to explore and confront somatic and
germline data with different metrics.  
- Enable users to combine data from multiple data sets to make
meaningful analyses and summaries of large  
cancer genetic studies.  

GeneApp is interactive. Results update immediately when inputs are
changed. GeneApp is a tool made with the aim of making life easier to
genetics researchers, so I am hopeful that it will be particularly
helpful in assisting in the genetic research effort.  

If you are interested to see what GeneApp has to offer, click
here.(link)  

## Enviroment setup and Installation

If you are in to test GeneApp from within R, run the following in your R
session to install the GeneApp package current development version from
Github. (not in a docker container)  
GeneApp works on Windows, Mac, or Linux. It can run without an Internet
connection and no data will leave your computer. You can also run the
app as a web application on a server.

- Required: [R](https://cran.r-project.org/) version 4.0.0 or later
- Required: [Rstudio](https://posit.co/download/rstudio-server/)

<!-- -->


    if (!require("devtools")) {
      install.packages("devtools")
    }

    install.packages(c("data.table", "dplyr", "DT", "kableExtra", "kit", "knitr", "maftools", "magrittr", 
    "purrr", "shiny", "shinydashboard", "shinyFiles", "shinyjs", "shinyWidgets", "stringr", "writexl"))

    devtools::install_github("FilippoMazzara/Progetto_IFO")

    library("geneApp") 

    devtools::load_all(".")

then to run the app simply call the launcher function
`geneApp::run_geneApp()` from the console.

unfortunately there is not yet a release available on CRAN.  

The development environment of this project will be encapsulated in a
Docker container.  
This will be the steps required to set it up.  

1.  Install Docker. Follow the instructions on
    <https://docs.docker.com/install/>  

2.  Make docker run without sudo  

        sudo groupadd docker
        sudo usermod -aG docker $USER

      
    Log out and log back in so that your group membership is
    re-evaluated  

3.  Clone the GIT repository  

        git clone https://github.com/FilippoMazzara/Progetto_IFO.git

      

4.  Setup development Docker container  

        cd Progetto_IFO
        bin/setup-environment.sh

      
    You should see lots of container build messages  

5.  Spin up the container  

        bin/start_rstudio.sh

      

6.  Open <http://localhost:8787> in your browser to start a new RStudio
    session  

7.  Install R packages required for this app. Run the following in your
    R session to install all the dependencies:  

        install.packages(c("data.table", "dplyr", "DT", "kableExtra", 
        "kit", "knitr", "maftools", "magrittr", "purrr", "shiny",     
        "shinydashboard", "shinyFiles", "shinyjs", "shinyWidgets", "stringr", "writexl"))

      
    The installation will take a few minutes.  

8.  Open the file `app.R` and hit the “Run app” button in the toolbar of
    the script editor (or type `geneApp::run_geneApp()` in the R session
    window). The Shiny app should open in a new window. You may need to
    instruct your browser to not block popup windows for this URL.  

## Basic Usage

When GeneApp starts you will see the blank overview page where you can
upload your data. To close the application close the browser window and
then click `Stop`. The GeneApp process will stop and the browser window
will close (Chrome) or gray-out. The main section of the app is the
overview page, this is where the users will find all the important
functions.  
The two main services are the comparison between somatic and germline
data sets and the combining of multiple different data sets. They mainly
differ in their purposes and in the way the data is presented but both
work with very similar UI elements and workflows, so I’ll quickly go
over them.

- From the sidebar a user can decide whether to upload a file/s from
  it’s local filesystem, or to chose from a set of files that are hosted
  on the server. In the [Advanced Usage](#advanced-usage) section you
  can find all the details about the formatting of files and the inner
  workings of the conversions.

  After a brief loading, if everything worked out, the chosen data
  should be ready to explore and you should have a few tools at your
  disposal, let’s see them:

- In the main panel of the selected view you’ll have the table
  containing your data in the center, you can navigate it and you are
  also able to select rows to copy, reorder the columns through a drag
  and drop and filter the records through the filters present on top of
  the table and in the sidebar.

- Both in the main panel and in the statistics panel you’ll find some
  useful and interactive stats and plots.

- From the sidebar you can chose to hide and show columns, show and then
  apply various filters and to export the explored data in various
  formats with a wide array of options.

All the UI elements should be pretty friendly and intuitive to use, if
you find yourself having problems there are some useful tool tips
sprinkled along the UI. In the Help page you’ll find more helpful tips
and all the useful links and contact information. The app should also be
working perfectly on mobile but there could be some quirks that are not
yet addressed.  

## Advanced Usage

For security and performance reasons the user uploaded files will only
be processed but not saved on the server that’s running the app. The
supported formats are the same on both the client and server sides, they
are: .csv .tsv .xls .xlsx .maf

When a file that is not already in maf standard is chosen the app will
try to turn it the closest it can to maf standard in order to interpret
the data more easily. If the transformation is not possible then the app
will still show the file but display a warning and many of the
funcionalities will not be available. If the file is not readable at all
it will show an error.  

If you are having problems visualizing your file you should check
whether they are maf compatible
[here](https://www.bioconductor.org/packages/devel/bioc/vignettes/maftools/inst/doc/maftools.html#1_Introduction).  
There is a possibility that with a few tweaks your file can also be
adapted to work fine in the app, still in many cases if your file is
using another genetic standard is very likely that you don’t need this
because the app will handle all the conversions for you!  

If you are still having problems or if you just want to make a
contribution for the project here are some of the inner workings of the
conversion:

All the column names in your files are tried to be matched with the maf
standard column names through these case insensitive pairing lists:

      ```
      Gene <- c("gene.refgene", "gene")
      Hugo_Symbol <- c("hugo_symbol") 
      Chromosome <- c("chr", "chrom", "chromosome")
      Reference_Allele <- c("reference_allele", "ref")
      Tumor_Seq_Allele2 <- c("tumor_seq_allele2", "alt")
      VAF <- c("vaf", "t_vaf")
      Variant_Classification <- c("variant_classification", "func.refgene")
      Variant_Type <- c("variant_type", "exonicfunc.refgene")
      VARIANT_CLASS <- c("variant_class")
      CLIN_SIG <- c("clin_sig", "clinvar")
      t_depth <- c("depth", "t_depth")
      Start_Position <- c("start_position", "start")
      End_Position <- c("end_position", "end")
      Existing_Variation <- c("existing_variation", "aachange.refgene", "variation", "var")
      HGVSp <- c("hgvsp") 
      EXON <- c("exon") 
      Tumor_Sample_Barcode <- c("tumor_sample_barcode")
      ```

If these don’t work a second round of conversion starts where other
pairings are applied in order to address different possible standards
and the missing columns that can be deducted from the other data are
computed like so:

      ```
      Hugo_Symbol <- c("symbol")
      Reference_Allele <- c("tumor_seq_allele1")
      HGVSp <- c("hgvsp_Short")
      EXON <- c("exon_number")
      ```

If not present the following will be tried to be inferred using values
from other columns, if they exist:  
- Tumor_Sample_Barcode is inferred from the file or the data set names  
- Vaf is calculated dividing t_depth for t_alt_count   - Variant_Type is
inferred from VARIANT_CLASS, Reference_Allele and Tumor_Seq_Allele2  
- Variant_Classification is inferred using Consequence and
Variant_Type  
- Most of the column types are reduced to the ones that correspond to
the maf standard and values formatting errors are tried to be fixed as
well, this is most significant when merging multiple data sets  

## Reporting issues

Please use the GitHub issue tracker at
<a href="https://github.com/FilippoMazzara/Progetto_IFO/issues" target="_blank">github.com/FilippoMazzara/Progetto_IFO/issues</a>
if you have any problems using Radiant.

## Development

This web application is written using the R Shiny web framework. It
makes use of custom HTML, CSS and JS,  
It also demonstrates the use various Rmarkdown templates, all in order
to create a fancy user experience.  
It is also encapsulated inside a Docker container for better portability
and control.  
The app was developed with best Shiny practices in mind, as the use of
Shiny modules, reactivity and fast server side processing, to name a
few. In total about 5000 lines of code were written for this app in a
few months.  
This time included app design and development but also all the required
research to program in R, a language that was foreign to me. Here are
listed just some of the invaluable sources I used to gather all this
knowledge:  

- [R for Data Science](https://r4ds.had.co.nz/index.html)  
- [Advanced R](http://adv-r.had.co.nz/)  
- [Mastering Shiny](https://mastering-shiny.org/index.html)  
- [Engineering Shiny](https://engineering-shiny.org/index.html)  
- [R Packages (2e)](https://r-pkgs.org/)  
- [R Markdown: The Definitive
  Guide](https://bookdown.org/yihui/rmarkdown/)  

## Further Information

This app was developed by Filippo Mazzara an informatics student from
the university “La Sapienza” in Rome, with the supervision of the
esteemed professor Andrea Sterbini. The patronage and the constant
assistance from the team of researchers from the highly regarded
“Hospital Physiotherapy Institutes” (IFO) were essential and defined the
shape of this project. Special thanks go to doctor Matteo Pallocca and
doctor Martina Betti from IFO whose help was crucial.  

For further information, please refer to the articles and function call
references of the package documentation, available at  

- GitHub: [PROGETTOIFO -
  Github](https://github.com/FilippoMazzara/Progetto_IFO)
- University “La Sapienza”: [Professor Andrea
  Sterbini](https://www.di.uniroma1.it/it/docenti/sterbini)
- Istituti Fisioterapici Ospitalieri: [IFO](https://www.ifo.it/)
- Linkedin: [Matteo
  Pallocca](https://it.linkedin.com/in/matteo-pallocca-b38742142)
- un mio contatto che non ho

Enjoy!
