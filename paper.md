---
title: 'nlsLoop'
tags:
  - non-linear regression
  - R
authors:
 - name: Daniel Padfield
   orcid: 0000-0000-0000-1234
   affiliation: "1"
affiliations:
 - name: University of Exeter
   index: 1
date: 14 February 2016
bibliography: paper.bib
---

# Summary

- __nlsLoop__ is a simple R package that gives a more reproducible and reliable method for fitting individual non-linear regression fits over levels of a factor. This procedure is commonly done using __nlme::nlsList__, but this function only uses one set of parameter values. Consequently, some curves fail to converge on the correct parameter values simply because of the starting values being too far away from the starting values. __nlsLoop__ improves on __nlme::nlsList__ by allowing multiple starting values for each parameter, thereby exploring more parameter space when model fitting. The best model is chosen based on AIC score, ensuring that results are more reproducible and replicable.
- This method and the R package __nlsLoop__ have been used in multiple publications in the last two years (Padfield _et al._ 2016, Padfield _et al._ 2017). It is also used in other projects within the lab group and is likely to be useful to the wider scientific community.

# References

- Padfield, D., Yvon-Durocher, G., Buckling, A., Jennings, S. & Yvon-Durocher, G. (2016). Rapid evolution of metabolic traits explains thermal adaptation in phytoplankton. Ecology Letters, 19, 133–142. doi:10.1111/ele.12545
- Padfield, D., Lowe, C., Buckling, A., Ffrench-Constant, R., Jennings, S., Shelley, F., et al. (2017). Metabolic compensation constrains the temperature dependence of gross primary production. Ecology Letters, 20, 1250–1260. doi:10.1111/ele.12820
