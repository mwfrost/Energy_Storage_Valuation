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
require(reshape)
```

```
## Loading required package: reshape
```

```
## Attaching package: 'reshape'
```

```
## The following object(s) are masked from 'package:plyr':
## 
## rename, round_any
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


```r
on.peak.rate.alt <- c(50, 60, 70, 80, 90, 100)
load.shift.duration.alt <- c(8, 9, 10, 11)

alt.vars <- expand.grid(on.peak.rate.alt = on.peak.rate.alt, load.shift.duration.alt = load.shift.duration.alt)

for (i in 1:nrow(alt.vars)) {
    alt.inputs <- inputs
    alt.inputs["on.peak.rate", "default"] <- alt.vars$on.peak.rate.alt[i]
    alt.inputs["load.shift.duration", "default"] <- alt.vars$load.shift.duration.alt[i]
    alt.vars$benefit[i] <- calc.benefits(alt.inputs)
}
alt.vars
```

```
##    on.peak.rate.alt load.shift.duration.alt benefit
## 1                50                       8  800000
## 2                60                       8  900000
## 3                70                       8 1000000
## 4                80                       8 1100000
## 5                90                       8 1200000
## 6               100                       8 1300000
## 7                50                       9  837500
## 8                60                       9  950000
## 9                70                       9 1062500
## 10               80                       9 1175000
## 11               90                       9 1287500
## 12              100                       9 1400000
## 13               50                      10  875000
## 14               60                      10 1000000
## 15               70                      10 1125000
## 16               80                      10 1250000
## 17               90                      10 1375000
## 18              100                      10 1500000
## 19               50                      11  912500
## 20               60                      11 1050000
## 21               70                      11 1187500
## 22               80                      11 1325000
## 23               90                      11 1462500
## 24              100                      11 1600000
```

```r
cast(alt.vars, on.peak.rate.alt ~ load.shift.duration.alt)
```

```
## Using benefit as value column.  Use the value argument to cast to override
## this choice
```

```
##   on.peak.rate.alt       8       9      10      11
## 1               50  800000  837500  875000  912500
## 2               60  900000  950000 1000000 1050000
## 3               70 1000000 1062500 1125000 1187500
## 4               80 1100000 1175000 1250000 1325000
## 5               90 1200000 1287500 1375000 1462500
## 6              100 1300000 1400000 1500000 1600000
```



