---
    title: "ggsupplyDemand"
    author: "David Salazar"
    date: "2018-05-19"
    output:
      html_document:
        keep_md: true
---

# What is it?

`ggsupplyDemand` is a package that makes it extremely easy to plot supply and demand with ggplot2.


```r
library(ggsupplyDemand)

create_supply_and_demand() %>% 
  shift_demand(outwards = TRUE) %>% 
  plot_supply_and_demand()
```

![](README_files/figure-html/unnamed-chunk-1-1.png)<!-- -->
