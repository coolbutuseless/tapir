
Tapir - Text-based Arbitrary Precision Integers in R
------------------------------------------------------------------------------





Installation
------------------------------------------------------------------------------


```r
devtools::install_github('coolbutuseless/tapir')
```



Rationale
------------------------------------------------------------------------------

This is a toy library written in order bootstrap another idea, without having to carry a dependency on a real
arbitrary precision library like [gmp](https://cran.r-project.org/package=gmp) or [Rmpfr](https://cran.r-project.org/package=Rmpfr).

The algorithms are straight-forward implementations of naive algorithms and are orders of magnitute slower than `gmp` even
for small/moderate sized integers.

Supported data types
------------------------------------------------------------------------------

The only supported datatype is an unsigned integer represented in a character string. e.g.

* "1234"
* "9237409238740928374029374293"



Supported operations
------------------------------------------------------------------------------

* `%add%` - Addition 
* `%sub%` - Subtration 
* `%sub%` - Multiplication
* `%div%` - Division 
* `%mod%` - Modulo operator
* Logical 
    * `%gt%`  - greater than 
    * `%gte%` - greater than or equal to
    * `%eq%`  - equal to
    * `%lt%`  - less than
    * `%lte%` - less than or equal to


Helper functions
------------------------------------------------------------------------------

* `rbigint(digits)`
    * Create a big integer with the given number of digits
    
    
    
Examples
------------------------------------------------------------------------------


```r
a <- rbigint(30)
b <- rbigint(20)
a
#> [1] "335928966021637479379261230383"
b
#> [1] "55418671748675570476"

a %mul% b
#> [1] "18616737098825116626650822383581769013422188972308"
a %div% b
#> [1] "6061656756"
a %mod% b
#> [1] "7731873551261694527"
a %add% b
#> [1] "335928966077056151127936800859"
a %sub% b
#> [1] "335928965966218807630585659907"
```
