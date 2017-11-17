# SWATsolaR
SWAT - SOiLgrids data Aggregation in R


## Installation

### Install required software
To use the *SWATsolaR* package requires some other software and *R* packages to install prior to the installation of *SWATsolaR*. To be able to access the *soilgrids* layers located on the ISRIC geoserver from within an *R* session the software **GDAL** is needed. The easiest way to obtain a version for 64bit Windows is to download it from this link ([gdal-201-1800-x64-core.msi](http://download.gisinternals.com/sdk/downloads/release-1800-x64-gdal-2-1-3-mapserver-7-0-4/gdal-201-1800-x64-core.msi)). Install **GDAL** with typical installation. If necessary, other ways to **GDAL** can be found [here](https://trac.osgeo.org/gdal/wiki/DownloadingGdalBinaries).
Further, to calculate additional soil parameters than the ones provided by *soilgrids* the **euptf** package is required. First, download the package following this link ([euptf.zip](http://eusoils.jrc.ec.europa.eu/public_path/shared_folder/themes/euptf.zip)). Unzip the folder of the **euptf** package and proceed with the installation in *R* running the following lines of code:
```{r}
# Install package dependencies
install.packages(c("rpart", "gWidgets", "gWidgetstcltk", "raster"))
# Install the euptf package (of course change path to your actual source path :))
install.packages("Path:/to/source_file/euptf_1.4.tar.gz", repos = NULL, type = "source")
```
### Install the SWATsolaR package
To install the *SWATsolaR* package first install the required dependencies and then install the recent version of the package from this *github* repository. The easiest way to install directly from a *github* repository is to use the *devtools* package (that I added in the installation of the dependencies).
```{r}
# Install package dependencies
install.packages(c("devtools", "gdal", "sp", "XML", "pasta")) # here I will add further dependencies when the package grows... 
# Install SWATsolaR
devtools::install_github("chrisschuerz/SWATsolaR")
```

### To be continued...
