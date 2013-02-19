Energy Storage Technology Valuation
========================================================

An R version of the method described in [Energy Storage Technology Valuation Primer](http://www.epri.com/abstracts/Pages/ProductAbstract.aspx?ProductId=000000000001008810) from EPRI.

## Deterministic Model

NPV is calculated in the `npv()` function in `func.r`

Source [here](http://tolstoy.newcastle.edu.au/R/help/05/12/16765.html)

   >t = c(-8000, 100, 100, 100, 2000, 3000, 4000, 5000)

   >r = 0.1

   >npv(r, t)

   >[1] 301.1624



```r
require(plyr)
```

```
## Loading required package: plyr
```

```r
require(ggplot2)
```

```
## Loading required package: ggplot2
```

```r
source("~/Code/energy_projects/func.r")

inputs <- data.frame(default = c(80, 20, 5, 5, 10, 250, 1e+05, 2))

row.names(inputs) <- c("on.peak.rate", "off.peak.rate", "demand.charge", "capacity", 
    "load.shift.duration", "load.shifting.periods", "cost.per.volt.sag", "volt.sag.count")
inputs$var.name <- row.names(inputs)

# From http://www.tybecenergy.com/pricehistory/pjm_settle.php
on.peak.rate <- 44.4  # $
off.peak.rate <- 31.13  # $


b.total <- calc.benefits(inputs)

sprintf("$%.f", b.total)
```

```
## [1] "$1250000"
```


Now exercise the model for sensitivity analysis


```r

percent(exercise(inputs, "capacity", 1.1)/b.total - 1)
```

```
## [1] "8.40%"
```

```r
percent(exercise(inputs, "on.peak.rate", 1.1)/b.total - 1)
```

```
## [1] "8.00%"
```

```r

df.delta10 <- ddply(inputs, .(var.name), function(x) {
    c(delta10 = (exercise(inputs, x$var.name, 1.1) - b.total)/b.total)
})

df.delta10 <- transform(df.delta10, var.name = reorder(var.name, delta10))

ggplot(df.delta10, aes(var.name, delta10)) + geom_bar(stat = "identity") + coord_flip() + 
    scale_y_continuous(labels = percent, limits = c(-0.05, 0.1))
```

```
## Warning: Stacking not well defined when ymin != 0
```

![plot of chunk figure.3-1](figure/figure.3-1.png) 


# Now examine the change in benefits over a range of different inputs



