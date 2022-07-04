Bayesian LCA
================
2022-07-02

## Latent Class Analysis

This latent class analysis project, we assume the indicator variables
are dichotomous.

### Simulating the dichotomous variables

Suppose there are
![M](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;M "M")
dichotomously scored items
![(Y_1, Y_2, Y_3, \\ldots, Y_M)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%28Y_1%2C%20Y_2%2C%20Y_3%2C%20%5Cldots%2C%20Y_M%29 "(Y_1, Y_2, Y_3, \ldots, Y_M)")
observed on
![N](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;N "N")
participants for measuring a latent trait,
![\\theta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Ctheta "\theta").
Based on the responses to these categorical indicators, the participants
can be categorized into
![c](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;c "c")
classes. Suppose that the
![c = \\{2, 3, \\dots, K\\}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;c%20%3D%20%5C%7B2%2C%203%2C%20%5Cdots%2C%20K%5C%7D "c = \{2, 3, \dots, K\}")
class have marginal probabilities
![P = \\{p_1, p_2, \\ldots, p_K \\}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;P%20%3D%20%5C%7Bp_1%2C%20p_2%2C%20%5Cldots%2C%20p_K%20%5C%7D "P = \{p_1, p_2, \ldots, p_K \}")
such that
![\\sum\_{c = 1}^{K} p_c = 1](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Csum_%7Bc%20%3D%201%7D%5E%7BK%7D%20p_c%20%3D%201 "\sum_{c = 1}^{K} p_c = 1")

``` r
library(data.table)
LCA_sim <- function(NC, NI, N, p, cp){
  p <- p/sum(p)
  cl <- sample(x = 1:NC, size = N, replace = TRUE, prob = p)
  y <- matrix(nrow = N, ncol = NI)
  
  for(i in 1:N){
    for(j in 1:NC) {
      if(cl[i] == j){
        y[i, ] <- rbinom(n = rep(1, NI), size = rep(1, NI), prob = cp[j,])
      }
    }
  }
  Y <- as.data.frame(y)
  colnames(Y)<- paste0("V", 1:NI)
  Y$group <- cl
  return(Y)
}

p <- c(.21, .62, .17)

cp <- matrix(data = c(.7, .35, .05, .09, .31, .06, .01, .05, .72, .49, .3, .62), 
             ncol = 4, nrow = 3, byrow = TRUE)

cp
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,] 0.70 0.35 0.05 0.09
    ## [2,] 0.31 0.06 0.01 0.05
    ## [3,] 0.72 0.49 0.30 0.62

Simulating a dataset

``` r
Y <- LCA_sim(NC = 3, NI = 4, N = 500, p = p, cp = cp)
Y <- data.table(Y)

Y[, lapply(.SD, mean), by = group, .SDcols = paste0("V", 1:4)][order(group),]
```

    ##    group        V1         V2          V3         V4
    ## 1:     1 0.7289720 0.33644860 0.037383178 0.03738318
    ## 2:     2 0.2918033 0.03934426 0.009836066 0.05573770
    ## 3:     3 0.7272727 0.54545455 0.295454545 0.60227273
