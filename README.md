# solACE
**soil** information **A**ggregation, **C**lustering and **E**stimation from soilgrids data.

solACE provides a workflow to download soilgrids from [soilgrids.org](https://soilgrids.org/) for adefined shapefile domain, calculates additional soil parameters, aggregates the input layers over the depth and clusters the soil layers in space to define soil groups.

## Installation
solACE is hosted [github](https://github.com/chrisschuerz/solACE) and can be isntalled from there (e.g. by using the `devtools` package). Here is how to do that:

```{r}
library(raster)
filename <- system.file("external/lux.shp", package="raster")
example_shape <- shapefile(filename)
plot(example_shape)
```

Initialize project:

```{r}
  library(solACE)
  new_soil_project("soilgrids",
                   "/Users/dan/Documents/solACE-demo",  
                    shape_file = example_shape)
```

The environment should now look like this:
<center>
![](readme/environ-01.png) 
</center>

And, a new folder should appear in your project path, like this: 
<center>
![](readme/folder-01.png) 
</center>

Lets load some maps:

```{r}
  soilgrids$load_soilgrids()
```

- gdal does not work on my laptop :( 
