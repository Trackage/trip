# Tools for animal track data

The trip package provides functions for accessing and manipulating
spatial data for animal tracking.  Filter for speed and create time
spent plots from animal track data.

## Installing

A) The package is easily installed from CRAN using 'install.packages("trip")' from R. 

OR 

B) The package can be easily built with RStudio. 

1. Install R

2. Install [RStudio](http://www.rstudio.com)

3. Install [Rtools](http://cran.r-project.org/bin/windows/Rtools/) or equivalent for your platform

4. Install [sp](http://cran.r-project.org/web/packages/sp/index.html),
           [spatstat](http://cran.r-project.org/web/packages/spatstat/index.html),
           [maptools](http://cran.r-project.org/web/packages/maptools/index.html),
           [adehabitatLT](http://cran.r-project.org/web/packages/adehabitatLT/index.html),
           packages and dependencies.

5. Clone the repository from GitHub (https://github.com/mdsumner/trip).

6. Create an Rstudio project in the folder containing this README file.

7. Click 'Build and Reload' to make the package immediately available
to RStudio, or 'Build Source Package' or `Build Binary Package' from
the 'Build' tab to make source or binary package.



## TODO

- **Probability image**.  The SGAT (and tripEstimation) packages have
functions for dealing with spatial track summaries that are
atomized to the level of each time step. There are methods for combining summaries from
multiple tracks and for casting arbitrary durations (by sum) to standard image structures. This would be a good
feature to replace the existing tripGrid function by storing the 
individual grid summaries for each implicit line segment.
