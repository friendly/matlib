
Announce: 
v 1.0.0 of the `matlib` package now supports constructing LaTeX matrix expressions
and equations directly in R, using either numeric or symbolic matrices.
The resulting LaTeX code can be directly included in an Rmarkdown/Quarto document.
A preview feature renders an equation in an RStudio Viewer pane.

For example, the SVD of a matrix X can be expressed as:

```
Eqn("\\mathbf{X} =",
    latexMatrix("u", "n", "k"),
    latexMatrix("\\lambda", "k", "k", diag=TRUE),
    latexMatrix("v", "k", "p", transpose = TRUE), label='eq:svdmats')
```

The attached image shows this in RStudio, with the preview pane.

![](man/figures/matlib-preview.png)

See the vignette, http://friendly.github.io/matlib/articles/latex-equations.html for other examples.
