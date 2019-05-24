**WARNING** Package in heavy development.   


# rCGM2

This R package proposes convenient methods for processing **3D gait dataset**  

rCGM2  relies on  Excel spreadsheets exported  from [pyCGM2](https:\\pycgm2.github.io). 
You can have a look into the folder *data/pyCGM2* to find out the structure of the spreadsheet. 
Briefly, each row respresents a time-normalized gait cycle and columns report either value from frame0 to 100 or events, phase duration and so on. 

The ongoing implementation allows to :

 * Plot data 
 * compare data based on basic metrics, like the Mean Absolute Deviation, the Root Mean Square Error ...
 * process repeatbility tests based on the *Standard Error of measurement* 

Since R is a statistical-oriented software, further versions of rCGM2 could  propose methods integrating basic inferential statistical tests.


## Why R is fantastic !

Originaly R was not fantastic at all. The user interface was not intuitive and the programming syntax was difficult to master. 
That is the past. Thanks to the enthusiast of R users and especially [Hadley Wickham](http://hadley.nz/), R now benefits from a powerful user interface
called [Rstudio](https://www.rstudio.com/) and packages which could ease your data processing. 

rCGM2 precisely leverages packages : 
  
  - dplyr
  - ggplot2
  - tidyR
  
There are extensive documentation and tutorials on the internet about these packages. For sure, they will accelerate your learning curve
and remove any reluctance to R. 



## installation

Download [Rstudio](https://www.rstudio.com/)

then type
```
library(devtools)
install_github("pyCGM2", "rCGM2")
```






