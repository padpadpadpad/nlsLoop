## nlsLoop

Tools expanding the non-linear regression method nls and nlsList from nlme.

### Issues and suggestions

Please report any issues/suggestions in the [issues link](https://github.com/padpadpadpad/nlsLoop/issues) for the repository.

Mac: [![Build Status](https://travis-ci.org/padpadpadpad/nlsLoop.svg?branch=master)](https://travis-ci.org/padpadpadpad/nlsLoop)

### Overview

The mainstay of this package is `nlsLoop::nlsLoop()`. If you have a dataset where you want to fit the same model over many levels of a factor, you may use `nlme::nlsList()`. However, `nlsList()` only allows for one set of starting values so its likely not all of the models will converge.

`nlsLoop()` allows for a range of starting values and tries unlimited different starting values to the fit at each level of the factor, picking the best fit for each model using AIC scores.

### To install :

`devtools::install_github("padpadpadpad/nlsLoop")`

OR

`ghit::install_github("padpadpadpad/nlsLoop")`


