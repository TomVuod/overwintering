The R code for the reasearch project published in:

[Włodarczyk T. 2021. Functional spatial distribution and sociometric characteristics of Formica fusca ants during winter dormancy. Ecological Entomology 46, 419–427.](https://resjournals.onlinelibrary.wiley.com/doi/10.1111/een.12983)

# Installation
```
if(!require(devtools)) install.packages("devtools")
devtools::install_github("TomVuod/overwintering", build_vignettes=TRUE)
```
# Run vignette
```
library(overwintering)
vignette("Data_workflow")
```
# Render plots
```
figure_5() # Fig.5 from the publication
figure_6() # Fig.6 

```


