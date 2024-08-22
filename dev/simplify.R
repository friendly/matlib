simplify <- function(string, simplify0s=TRUE, simplify1s=TRUE, 
                     sparse=FALSE, ...){
  UseMethod("simplify")
}

simplify.default <- function(string, simplify0s=TRUE, simplify1s=TRUE, 
                             sparse=FALSE, ...){
  if (simplify1s){
    # remove leading 1
    string <- gsub(" *[^0-9]1 *\\\\cdot *", " ", 
                   paste0(" ", string) )
    # removing trailing 1
    string <- gsub(" *\\\\cdot *1 {1,}", " ", 
                   paste0(string, " ")) 
  }
  if (simplify0s){
    # remove items with leading 0
    string <- gsub(" *[^1-9_{}}]*0 \\\\cdot (\\(*.*\\))* *[^+-]*", 
                   " ", string)
    # remove items with trailing 0
    string <- gsub(" *(\\(*.*\\))*.*\\\\cdot *[^1-9_{}}]*0", 
                   " ", string) 
    if (sparse){
      string <- trimws(string)
        string <- sub("^0$", "", string)
      } else {
      string <- sub("^ *$", "0", string) # replace empty with 0
    }
  }
  string <- sub("^ *\\+ *", "", string) # remove stray + 
  string <- trimws(string)
  string
}

if (FALSE){
  
cell <- "0 \\cdot \\beta_{0,y_{1}} + 1 \\cdot \\beta_{1,y_{1}} + 0 \\cdot \\beta_{2,y_{1}} + 0 \\cdot \\beta_{3,y_{1}}"
cell
simplify(cell)
simplify("0 \\cdot (a + b)")
simplify("0 \\cdot \\beta_{1,y_{1}}")
simplify("1 \\cdot (a + b)")
simplify("1 \\cdot \\beta_{1,y_{1}}")
simplify("\\beta_{1,y_{1}} \\cdot 1") 
simplify("\\beta_{1,y_{1}} \\cdot 0")
simplify("(a + b) \\cdot 1")
simplify("(a + b) \\cdot 0")
simplify("0 \\cdot (a + b)", sparse=TRUE)
simplify("(a + b) \\cdot 1", sparse=TRUE)
}
