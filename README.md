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

since R is a statistical-oriented software, further versions of rCGM2 could propose basic inferential statistical tests.

## installation

Download [Rstudio](https://www.rstudio.com/)

then type
```
library(devtools)
install_github("pyCGM2", "rCGM2")
```






