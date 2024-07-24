---
title: "R Markdown: Display a Matrix for R Variable"
author: "Romain Lesur"
output: 
  html_document:
    keep_md: true
---

This is from:
[StackOverflow](https://stackoverflow.com/questions/45591286/for-r-markdown-how-do-i-display-a-matrix-from-r-variable)







``` r
A = matrix(c(1,3,0,1),2,2)
B = matrix(c(5,3,1,4),2,2)
```


Now, matrix are rendered as LaTeX:

Matrix A in inline mode: $\begin{bmatrix} 1 &0 \\3 &1 \\ \end{bmatrix}$

Matrix A in display mode:

$$\begin{bmatrix} 1 &0 \\3 &1 \\ \end{bmatrix}$$

### Operators

As other answers suggested, it could be useful to define operators.  
With the previous class, it is relatively straightforward:



Example in inline mode: $\begin{bmatrix} 1 &0 \\3 &1 \\ \end{bmatrix} + \begin{bmatrix} 1 &0 \\3 &1 \\ \end{bmatrix} \times \begin{bmatrix} 5 &1 \\3 &4 \\ \end{bmatrix}$

Display mode:
$$\begin{bmatrix} 1 &0 \\3 &1 \\ \end{bmatrix} \times \begin{bmatrix} 5 &1 \\3 &4 \\ \end{bmatrix}$$
