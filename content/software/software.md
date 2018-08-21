+++
date = 2017-10-08
lastmod = 2017-10-08
draft = false
title = "Software"
math = true
summary = ""
+++

I am the author and developer of the R package "puniform" that contains meta-analysis methods to correct for publication bias. Three methods are currently included in the package. First, the *p*-uniform method ([van Assen, van Aert, & Wicherts, 2015]({{< ref "publication/punifom_psych_methods.md" >}}); [van Aert, Wicherts, & van Assen, 2016]({{< ref "publication/puniform_perspectives.md" >}})) can be used for estimating effect size, testing the null hypothesis of no effect, and testing for publication bias in a meta-analysis. Another method in the package is the hybrid method ([van Aert & van Assen, 2017]({{< ref "publication/hybrid.md" >}})). This method is a meta-analysis method for combining an original study and replication and while taking into account statistical significance of the original study. The *p*-uniform and hybrid method are based on the statistical theory that the distribution of *p*-values is uniform at the population effect size. The third method in the package is the Snapshot Bayesian Hybrid Meta-Analysis Method ([van Aert & van Assen, 2017]({{< ref "publication/bayesian_snapshot.md" >}})). This method computes posterior probabilities at four hypothesized effect sizes (e.g., no, small, medium, and large) based on an original study and replication while taking into account statistical significance in the original study. The method can also be used for computing the required sample size of the replication akin to power analysis in null hypothesis significance testing.

### Installing the "puniform" package

The "puniform" package is available on [CRAN](https://cran.r-project.org/web/packages/puniform/index.html), and it can be easily installed and loaded into R. The following lines of code install and load the package into a R session:

```python
### Install puniform package
install.packages("puniform")

### Load puniform package
library(puniform)

### Access documentation
?puniform
?hybrid
?snapshot
```

### Installing the development version of the "puniform" package

It is also possible to install and load the development version of the package. This version of the package is available on [Github](https://github.com/RobbievanAert/puniform) and contains the latest changes. The following lines of code can be used to install and load the development version into a R session:

```python
### Install devtools package
install.packages("devtools")

### Install puniform package
devtools::install_github("RobbievanAert/puniform")

### Load puniform package
library(puniform)
```

### Web applications

I also developed web applications for applying [*p*-uniform](https://rvanaert.shinyapps.io/p-uniform), the [hybrid method](https://rvanaert.shinyapps.io/hybrid), and [Snapshot Bayesian Hybrid Meta-Analysis Method](https://rvanaert.shinyapps.io/snapshot) for researchers who are not familiar with R.
