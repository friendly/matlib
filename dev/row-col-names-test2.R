source(here::here("dev", "row-col-names-test.R"))

# longer row/col labels

mat <- matrix(sample(25), nrow = 5,
              dimnames = list(who = c("Abe", "Bart", "Cat", "Doug", "Eve"),
                              what = c("Geog", "Hist", "Math", "Read", "Spell")))


# should latexMatrix recognize that it has row/col names?
latexMatrix(mat)

# alignment of col labels quite off
latexMatrix(mat, 
            rownames = paste("\\text{",rownames(mat),"}"), 
            colnames = paste("\\text{",rownames(mat),"}"))

# make cell entries wider
latexMatrix(10000*mat, 
            rownames = paste("\\text{",rownames(mat),"}"), 
            colnames = paste("\\text{",colnames(mat),"}"))
