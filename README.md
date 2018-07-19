# fsurfR
Author: T.R. Koscik, timkoscik+fsurfr@gmail.com  
Date: July 2018  
Copyright (C) 2018 Koscik, Timothy R. All Rights Reserved  

## Utilities for Freesurfer MGH,CURV,SURF File I/O and Vertex-wise Analysis

****

## To install this package, use devtools.
```
devtools::install_github("TKoscik/fsurfR")
```

****

## Below is an example script to conduct vertexwise analyses using the fsurfR package.

### Clear environment
```
rm(list=ls())
gc()
```

### Load necessary and desired libraries
```
library(fsurfR)
library(parallel)
library(doParallel)
library(car)
```

### Initialize variables
```
data.dir <- "/freesurfer/subjects/directory"
save.dir <- "/location/to/save/output"
prefix <- "vertex.analysis.lh" # will be appended to output file names,
                               # often will want to include which hemisphere
pf <- read.csv("/location/of/paradigm.variables.csv")
which.hemi <- "rh"
surf.file <- paste0(data.dir, "/fsaverage/surf/", which.hemi, ".sphere.reg")
which.sjx <- "all"
var.name <- c("area", "thickness")
n.vtx <- read.surf(surf.file)$n.vertex
tolerance <- 1 # maximum distance to vertex to calculate weighted average
```

### Set up models
#### freesurfer variables will be named by hemisphere then the variable name
#### e.g., rh.thickness, rh.area, rh.curv
```
FORM <- c(formula(paste0(which.hemi, ".thickness ~ group + age + sex")), 
          formula(paste0(which.hemi, ".area ~ group + age + sex")))
```

### Code within model.fxn (below) will be run vertexwise
```
model.fxn <- function(X, ...) {
  vtx.vals <- load.surf.group(data.dir, which.sjx, which.hemi,
                              var.name, X, tolerance)
  df <- data.frame(vtx.vals, pf)
  
  mdl1 <- lm(FORM[[1]], df)
  thick.coef <- as.data.frame(summary(mdl1)$coef)
  thick.aov <- Anova(mdl1, type=3L)
  table.to.curv(thick.coef, X, surf.file, coords=NULL, save.dir, prefix, mdl1)
  table.to.curv(thick.aov, X, surf.file, coords=NULL, save.dir, prefix, mdl1)
  
  mdl2 <- lm(FORM[[2]], df)
  area.coef <- as.data.frame(summary(mdl2)$coef)
  area.aov <- Anova(mdl2, type=3L)
  table.to.curv(area.coef, X, surf.file, coords=NULL, save.dir, prefix, mdl2)
  table.to.curv(area.aov, X, surf.file, coords=NULL, save.dir, prefix, mdl2)
}
```

### Setup parallelization
```
num.cores <- detectCores()
registerDoParallel(num.cores)
```

### Run models
```
foreach(X=1:n.vtx) %dopar% model.fxn(X)
```

### Stop parallelization
```
stopImplicitCluster()
```
