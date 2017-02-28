<!-- README.md is generated from README.Rmd. Please edit that file -->
docaids
=======

R code is often first generated in a haphazard and exploratory way and this package helps to automatically discover and document the structure of local variables in functions. This makes it easier to both understand the existing code and document it in roxygen. It currently provides 3 main services, though all 4 are interrelated.

-   Functions inserted into other functions you want to document
    -   **doc\_vars\_in\_this\_func** is the function that does most of the work. It finds all of the variables defined inside the function where it is called and then runs the R function "str" on each of them to show the structure of each variable.
    -   **doc\_vars\_in\_this\_func\_once** is a wrapper function that can be used in place of *doc\_vars\_in\_this\_func* when you want to make sure that *doc\_vars\_in\_this\_func* is only called one time for any given function, no matter how many times that function is called in a program. The intent here is to avoid lots of duplicate output because you generally only need one copy of the structures.
-   **bump\_global\_ctr\_for\_cur\_func** is a support function for *doc\_vars\_in\_this\_func\_once*, though it can be used on its own if you want to count the number of times a function is called during a run of a program. It simply creates a unique global counter for the function at run time if it doesn't already exist and then increments that counter each time the function is called.
-   Function run on output from functions above
    -   **generate\_func\_var\_roxygen\_comments** is a function that reads the output generated by *doc\_vars\_in\_this\_func* and formats the information to write it back out in a form that can be pasted into the roxygen documentation at the start of a function.

Installation
------------

You can install docaids from github with:

``` r
# install.packages ("devtools")  
devtools::install_github ("langfob/docaids")
```

Example
-------

This is a basic example which shows you how to solve a common problem:
- (An example that shows how to use the package to solve a simple problem.)

``` r
...
```

Overview
--------

-   (An overview that describes the main components of the package. For more complex packages, this will point to vignettes for more details.)
