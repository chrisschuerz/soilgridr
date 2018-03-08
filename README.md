# solACE
**soil** information **A**ggregation, **C**lustering and **E**stimation from soilgrids data


## Installation

### Install required software
The *solACE* package requires some additional software and R packages (which needs to be installed prior to the installation of *solACE*). 

The software **GDAL** is needed to to access the *soilgrids* layers on the ISRIC geoserver from within an *R* session. The easiest way to obtain a version for 64bit Windows is to download it from this [link](http://download.gisinternals.com/sdk/downloads/release-1800-x64-gdal-2-1-3-mapserver-7-0-4/gdal-201-1800-x64-core.msi). Just install it with *typical installation*. Other options to obtain **GDAL** can be found [here](https://trac.osgeo.org/gdal/wiki/DownloadingGdalBinaries).

Further, to calculate additional soil parameters (than the ones provided by *soilgrids*) the **euptf** package is required. First, download the package from [this link](http://eusoils.jrc.ec.europa.eu/public_path/shared_folder/themes/euptf.zip); then  unzip the folder of the **euptf** package and proceed with the installation in *R*. The latter can be done by running the following lines of code from within *R*:
```{r}
# Install package dependencies
install.packages(c("rpart", "gWidgets", "gWidgetstcltk", "raster"))
# Install the euptf package (of course change path to your actual source path :))
install.packages("Path:/to/source_file/euptf_1.4.tar.gz", repos = NULL, type = "source")
```

### Install the solACE package
After the required dependencies are installed the recent version of the **solACE** package can be obtained from this *github* repository. The easiest way to get it is to directly install it from the *github* repository by using the *devtools* package. Here is how you can do that:
```{r}
# Install package dependencies
install.packages(c("devtools", "dplyr", "ggplot2", "magrittr", "pasta", "purrr", "raster", "rgdal", "sp", "tibble", "XML"))
# Install SWATsolaR
devtools::install_github("chrisschuerz/SWATsolaR")
```

### To be continued...
