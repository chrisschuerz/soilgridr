# SWATsolaR
SWAT - Soilgrids data aggregation in R


## Installation
To use the *SWATsolaR* package requires some other software and R packages to install prior to the installation of *SWATsolaR*. To be able to access the *soilgrids* layers located on the ISRIC geoserver from within an R session the software **GDAL** is needed. The easiest way to obtain a version for 64bit Windows is to download it from this link ([gdal-201-1800-x64-core.msi](http://download.gisinternals.com/sdk/downloads/release-1800-x64-gdal-2-1-3-mapserver-7-0-4/gdal-201-1800-x64-core.msi)). Other ways to **GDAL** can be found [here](https://trac.osgeo.org/gdal/wiki/DownloadingGdalBinaries).
Further, to calculate additional soil parameters than the ones provided by *soilgrids* the **euptf** package is required. First, download the package following this link ([euptf.zip](http://eusoils.jrc.ec.europa.eu/public_path/shared_folder/themes/euptf.zip)). Unfortunately, the **euptf** package is not properly maintained and requieres also the **mvpart** package that is not available from *CRAN* anymore. Get the package from this link ([mvpart](https://cran.r-project.org/src/contrib/Archive/mvpart/mvpart_1.6-2.tar.gz). Unzip the folder of the **euptf** package and proceed with the installation in R package running the following lines of code:
```{r}



```
