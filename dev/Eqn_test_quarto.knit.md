---
title: "Using Eqn() and ref() test with Quarto"
engine: knitr
---

::: {.cell}

```{.r .cell-code}
library(matlib)
library(car)
```

::: {.cell-output .cell-output-stderr}

```
Loading required package: carData
```


:::
:::



Quarto has it's own syntax for creating displayed equations with equation numbers and a way to refer to them in text.
We illustrate these here.

## Simple equations
A standard equation and reference, as a displayed equation in Quarto:

$$
f\left(k\right) = \binom{n}{k} p^k\left(1-p\right)^{n-k}
$$ {#eq-binom}

This can be referenced as: See @eq-binom, and [-@eq-binom]

Syntax generated with `Eqn()`. Raw output:



::: {.cell}

```{.r .cell-code}
Eqn("f\\left(k\\right) = \\binom{n}{k} p^k\\left(1-p\\right)^{n-k}",
    quarto=TRUE)
```

::: {.cell-output .cell-output-stdout}

```

$$
f\left(k\right) = \binom{n}{k} p^k\left(1-p\right)^{n-k}
$$ 
```


:::
:::



Using `results: 'asis'` in the chunk renders this as:




```{.r .cell-code}
Eqn("f\\left(k\\right) = \\binom{n}{k} p^k\\left(1-p\\right)^{n-k}",
    quarto=TRUE)
```


$$
f\left(k\right) = \binom{n}{k} p^k\left(1-p\right)^{n-k}
$$ 



Add an equation label and number:




```{.r .cell-code}
Eqn("f\\left(k\\right) = \\binom{n}{k} p^k\\left(1-p\\right)^{n-k}",
    quarto=TRUE, label='eq-binom2')
```


$$
f\left(k\right) = \binom{n}{k} p^k\left(1-p\right)^{n-k}
$$ {#eq-binom2}



## Inline equation referencing.

- Manually with Quarto syntax (works): @eq-binom and @eq-binom2
- With `matlib` using `ref(., quarto=TRUE)`: ([-@eq-binom]) and ([-@eq-binom2])

Will also work within an equation, not that this is particularly useful.
    



```{.r .cell-code}
"@eq-binom"
```

[1] "@eq-binom"



## Auto use `quarto=TRUE` if .qmd file used

Internally, set global to TRUE if .qmd file detected. Error raised if files have the same name but different extension (`Eqn.qmd` and `Eqn.Rmd` will raise error if in same dir).





```{.r .cell-code}
Eqn("f\\left(k\\right) = \\binom{n}{k} p^k\\left(1-p\\right)^{n-k}", label = 'eq-auto')
```


$$
f\left(k\right) = \binom{n}{k} p^k\left(1-p\right)^{n-k}
$$ {#eq-auto}




See ([-@eq-auto]) for auto-detected quarto label. Of course, the @ notation still works too; e.g., see @eq-auto for details

## Testing `latexMatrix()`

Even this simple test doesn't work:



::: {.cell}

```{.r .cell-code}
Eqn("\\mathbf{X}=", latexMatrix(), quarto=TRUE)
```

::: {.cell-output .cell-output-stdout}

```

$$
\mathbf{X}=\begin{pmatrix} 
  x_{11} & x_{12} & \cdots & x_{1m} \\ 
  x_{21} & x_{22} & \cdots & x_{2m} \\ 
  \vdots & \vdots &        & \vdots \\ 
  x_{n1} & x_{n2} & \cdots & x_{nm} \\ 
\end{pmatrix}

$$ 
```


:::
:::



Rendered:




```{.r .cell-code}
Eqn("\\mathbf{X}=", latexMatrix(), quarto=TRUE)
```


$$
\mathbf{X}=\begin{pmatrix} 
  x_{11} & x_{12} & \cdots & x_{1m} \\ 
  x_{21} & x_{22} & \cdots & x_{2m} \\ 
  \vdots & \vdots &        & \vdots \\ 
  x_{n1} & x_{n2} & \cdots & x_{nm} \\ 
\end{pmatrix}

$$ 




## A more complex example

Here, we illustrate the decomposition of sums of squares and product (SSP) matrices in a simple one-way MANOVA design.


::: {.cell}

```{.r .cell-code}
data(dogfood, package = "heplots")
str(dogfood)
```

::: {.cell-output .cell-output-stdout}

```
'data.frame':	16 obs. of  3 variables:
 $ formula: Factor w/ 4 levels "Old","New","Major",..: 1 1 1 1 2 2 2 2 3 3 ...
 $ start  : int  0 1 1 0 0 1 2 3 1 5 ...
 $ amount : int  100 97 88 92 95 85 82 89 77 84 ...
```


:::

```{.r .cell-code}
#fit the model
dogfood.mod <- lm(cbind(start, amount) ~ formula, data=dogfood)
```
:::



Do the MANOVA tests and extract the SSP_H and SSP_E:



::: {.cell}

```{.r .cell-code}
dogfood.aov <- Anova(dogfood.mod)
SSP_H <- dogfood.aov$SSP[["formula"]]  |> print()
```

::: {.cell-output .cell-output-stdout}

```
          start   amount
start    9.6875 -70.9375
amount -70.9375 585.6875
```


:::

```{.r .cell-code}
SSP_E <- dogfood.aov$SSPE              |> print()
```

::: {.cell-output .cell-output-stdout}

```
       start amount
start  25.75  11.75
amount 11.75 390.25
```


:::

```{.r .cell-code}
SSP_T <- SSP_H + SSP_E
```
:::



Now, show that `SSP_T = SSP_H + SSP_E` numerically. As generated, each of these matrices have `rownames()` and `colnames()`. 



::: {.cell}

```{.r .cell-code}
options(print.latexMatrix = list(display.labels=FALSE))
options(digits = 4)
Eqn(latexMatrix(SSP_T), "=", latexMatrix(SSP_H), "+", latexMatrix(SSP_E),
    quarto = TRUE)
```

::: {.cell-output .cell-output-stdout}

```

$$
\begin{pmatrix} 
 35 & -59 \\ 
-59 & 976 \\ 
\end{pmatrix}
=\begin{pmatrix} 
  9.7 & -70.9 \\ 
-70.9 & 585.7 \\ 
\end{pmatrix}
+\begin{pmatrix} 
 26 &  12 \\ 
 12 & 390 \\ 
\end{pmatrix}

$$ 
```


:::
:::



Rendered by Quarto:




```{.r .cell-code}
Eqn(latexMatrix(SSP_T), "=", latexMatrix(SSP_H), "+", latexMatrix(SSP_E),
    quarto = TRUE)
```


$$
\begin{pmatrix} 
 35 & -59 \\ 
-59 & 976 \\ 
\end{pmatrix}
=\begin{pmatrix} 
  9.7 & -70.9 \\ 
-70.9 & 585.7 \\ 
\end{pmatrix}
+\begin{pmatrix} 
 26 &  12 \\ 
 12 & 390 \\ 
\end{pmatrix}

$$ 

