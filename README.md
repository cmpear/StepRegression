# StepRegression
<!-- badges: start -->
[![Travis build status](https://travis-ci.com/cmpear/StepRegression.svg?branch=master)](https://travis-ci.com/cmpear/StepRegression)
<!-- badges: end -->

This package is for examining how the residuals change as one adds each successive variable in a multilinear regression.

The package works by regressing the independent variables one at a time from both the dependent variable and the remaining independent variables.  The focus is on how the residuals change with the addition of each successive variable.

This package also includes some simple visualization functions for displaying how residuals change as each successive variable is regressed from the others.
