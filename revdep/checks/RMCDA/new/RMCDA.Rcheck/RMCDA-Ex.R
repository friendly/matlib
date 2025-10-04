pkgname <- "RMCDA"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('RMCDA')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("apply.AHP")
### * apply.AHP

flush(stderr()); flush(stdout())

### Name: apply.AHP
### Title: Apply AHP on the matrices
### Aliases: apply.AHP

### ** Examples

data <- read.csv(system.file("extdata", "AHP_input_file.csv", package = "RMCDA"), header=FALSE)
mat.lst <- read.csv.AHP.matrices(data)
mat.lst[[1]]->A
mat.lst[[2]]->comparing.competitors
results<- apply.AHP(A, comparing.competitors)



cleanEx()
nameEx("apply.ANP")
### * apply.ANP

flush(stderr()); flush(stdout())

### Name: apply.ANP
### Title: Apply Analytical Network Process (ANP) on data
### Aliases: apply.ANP

### ** Examples

data <- read.csv(system.file("extdata", "AHP_input_file.csv", package = "RMCDA"), header=FALSE)
mat.lst <- read.csv.AHP.matrices(data)
mat.lst[[1]]->A
mat.lst[[2]]->comparing.competitors
apply.ANP(A, comparing.competitors, 2)



cleanEx()
nameEx("apply.ARAS")
### * apply.ARAS

flush(stderr()); flush(stdout())

### Name: apply.ARAS
### Title: Apply Additive Ratio Assessment (ARAS)
### Aliases: apply.ARAS

### ** Examples


mat <- matrix(c(75.5, 95, 770, 187, 179, 239, 237,
420, 91, 1365, 1120, 875, 1190, 200,
74.2, 70, 189, 210, 112, 217, 112,
2.8, 2.68, 7.9, 7.9, 4.43, 8.51, 8.53,
21.4, 22.1, 16.9, 14.4, 9.4, 11.5, 19.9,
0.37, 0.33, 0.04, 0.03, 0.016, 0.31, 0.29,
0.16, 0.16, 0.08, 0.08, 0.09, 0.07, 0.06), nrow=7)
colnames(mat)<-c("Toughness Index",	"Yield Strength",	"Young's Modulus",
                 "Density",	"Thermal Expansion",	"Thermal Conductivity",	"Specific Heat")
rownames(mat)<-c("AI 2024-T6", "AI 5052-O","SS 301 FH",
"SS 310-3AH",
"Ti-6AI-4V",
"Inconel 718",
"70Cu-30Zn")
weights <- c(0.28, 0.14, 0.05, 0.24, 0.19, 0.05, 0.05)
beneficial.vector<-c(1,2,3)
apply.ARAS(mat, weights, beneficial.vector)



cleanEx()
nameEx("apply.BORDA")
### * apply.BORDA

flush(stderr()); flush(stdout())

### Name: apply.BORDA
### Title: Function to apply BORDA method to data
### Aliases: apply.BORDA

### ** Examples

# Create a small decision matrix (4 alternatives x 3 criteria)
mat <- matrix(c(
  5, 9, 2,
  7, 3, 8,
  6, 5, 4,
  4, 7, 9
), nrow = 4, byrow = TRUE)

beneficial.vector <- c(1, 3)


borda_scores <- apply.BORDA(mat, beneficial.vector)
borda_scores



cleanEx()
nameEx("apply.BWM")
### * apply.BWM

flush(stderr()); flush(stdout())

### Name: apply.BWM
### Title: Function for applying the Best-Worst Method
### Aliases: apply.BWM

### ** Examples

c <- c("C1", "C2", "C3")
w <- "C1"
b <- "C3"
bcp <- c(8, 2, 1)
wcp <- c(1, 5, 8)
apply.BWM(c, w, b, bcp, wcp)



cleanEx()
nameEx("apply.CILOS")
### * apply.CILOS

flush(stderr()); flush(stdout())

### Name: apply.CILOS
### Title: Apply CILOS Weighting Method
### Aliases: apply.CILOS

### ** Examples


mat <- matrix(
  c(75.5, 95, 770, 187, 179, 239, 237,
    420, 91, 1365, 1120, 875, 1190, 200,
    74.2, 70, 189, 210, 112, 217, 112,
    2.8, 2.68, 7.9, 7.9, 4.43, 8.51, 8.53,
    21.4, 22.1, 16.9, 14.4, 9.4, 11.5, 19.9,
    0.37, 0.33, 0.04, 0.03, 0.016, 0.31, 0.29,
    0.16, 0.16, 0.08, 0.08, 0.09, 0.07, 0.06),
  nrow = 7, byrow = TRUE
)
beneficial.vector <- c(1, 2, 3, 6, 7)
apply.CILOS(mat, beneficial.vector)



cleanEx()
nameEx("apply.COCOSO")
### * apply.COCOSO

flush(stderr()); flush(stdout())

### Name: apply.COCOSO
### Title: Apply COmbined COmpromise SOlution (COCOSO)
### Aliases: apply.COCOSO

### ** Examples


mat <- matrix(c(75.5, 95, 770, 187, 179, 239, 237,
420, 91, 1365, 1120, 875, 1190, 200,
74.2, 70, 189, 210, 112, 217, 112,
2.8, 2.68, 7.9, 7.9, 4.43, 8.51, 8.53,
21.4, 22.1, 16.9, 14.4, 9.4, 11.5, 19.9,
0.37, 0.33, 0.04, 0.03, 0.016, 0.31, 0.29,
0.16, 0.16, 0.08, 0.08, 0.09, 0.07, 0.06), nrow=7)
colnames(mat)<-c("Toughness Index",	"Yield Strength",	"Young's Modulus",
                 "Density",	"Thermal Expansion",	"Thermal Conductivity",	"Specific Heat")
rownames(mat)<-c("AI 2024-T6", "AI 5052-O","SS 301 FH",
"SS 310-3AH",
"Ti-6AI-4V",
"Inconel 718",
"70Cu-30Zn")
weights <- c(0.28, 0.14, 0.05, 0.24, 0.19, 0.05, 0.05)
beneficial.vector<-c(1,2,3)
apply.COCOSO(mat, weights, beneficial.vector)



cleanEx()
nameEx("apply.CODAS")
### * apply.CODAS

flush(stderr()); flush(stdout())

### Name: apply.CODAS
### Title: Apply Combinative Distance-based Assessment (CODAS)
### Aliases: apply.CODAS

### ** Examples


mat <- matrix(c(75.5, 95, 770, 187, 179, 239, 237,
420, 91, 1365, 1120, 875, 1190, 200,
74.2, 70, 189, 210, 112, 217, 112,
2.8, 2.68, 7.9, 7.9, 4.43, 8.51, 8.53,
21.4, 22.1, 16.9, 14.4, 9.4, 11.5, 19.9,
0.37, 0.33, 0.04, 0.03, 0.016, 0.31, 0.29,
0.16, 0.16, 0.08, 0.08, 0.09, 0.07, 0.06), nrow=7)
colnames(mat)<-c("Toughness Index",	"Yield Strength",	"Young's Modulus",
                 "Density",	"Thermal Expansion",	"Thermal Conductivity",	"Specific Heat")
rownames(mat)<-c("AI 2024-T6", "AI 5052-O","SS 301 FH",
"SS 310-3AH",
"Ti-6AI-4V",
"Inconel 718",
"70Cu-30Zn")
weights <- c(0.28, 0.14, 0.05, 0.24, 0.19, 0.05, 0.05)
beneficial.vector<-c(1,2,3)
psi <- 0.02
apply.CODAS(mat, weights, beneficial.vector, psi)



cleanEx()
nameEx("apply.COPELAND")
### * apply.COPELAND

flush(stderr()); flush(stdout())

### Name: apply.COPELAND
### Title: Apply Copeland Method
### Aliases: apply.COPELAND

### ** Examples

mat <- matrix(c(80, 60, 90,
                75, 85, 95,
                70, 65, 85,
                60, 75, 80),
              nrow = 4, byrow = TRUE)
colnames(mat) <- c("Criterion 1", "Criterion 2", "Criterion 3")
beneficial.vector <- c(1, 2, 3)
apply.COPELAND(mat, beneficial.vector)



cleanEx()
nameEx("apply.COPRAS")
### * apply.COPRAS

flush(stderr()); flush(stdout())

### Name: apply.COPRAS
### Title: Apply COmplex PRoportional ASsessment (COPRAS) method
### Aliases: apply.COPRAS

### ** Examples

mat <- matrix(c(75.5, 95, 770, 187, 179, 239, 237,
420, 91, 1365, 1120, 875, 1190, 200,
74.2, 70, 189, 210, 112, 217, 112,
2.8, 2.68, 7.9, 7.9, 4.43, 8.51, 8.53,
21.4, 22.1, 16.9, 14.4, 9.4, 11.5, 19.9,
0.37, 0.33, 0.04, 0.03, 0.016, 0.31, 0.29,
0.16, 0.16, 0.08, 0.08, 0.09, 0.07, 0.06), nrow=7)
colnames(mat)<-c("Toughness Index",	"Yield Strength",	"Young's Modulus",
"Density",	"Thermal Expansion",	"Thermal Conductivity",	"Specific Heat")
rownames(mat)<-c("AI 2024-T6",
"AI 5052-O",
"SS 301 FH",
"SS 310-3AH",
"Ti-6AI-4V",
"Inconel 718",
"70Cu-30Zn")
weights <- c(0.28, 0.14, 0.05, 0.24, 0.19, 0.05, 0.05)
beneficial.vector<-c(1,2,3)
apply.COPRAS(mat, weights, beneficial.vector)



cleanEx()
nameEx("apply.CRADIS")
### * apply.CRADIS

flush(stderr()); flush(stdout())

### Name: apply.CRADIS
### Title: Function to apply CRiteria Aggregation for Decision Information
###   Synthesis (CRADIS)
### Aliases: apply.CRADIS

### ** Examples


mat <- matrix(c(75.5, 95, 770, 187, 179, 239, 237,
420, 91, 1365, 1120, 875, 1190, 200,
74.2, 70, 189, 210, 112, 217, 112,
2.8, 2.68, 7.9, 7.9, 4.43, 8.51, 8.53,
21.4, 22.1, 16.9, 14.4, 9.4, 11.5, 19.9,
0.37, 0.33, 0.04, 0.03, 0.016, 0.31, 0.29,
0.16, 0.16, 0.08, 0.08, 0.09, 0.07, 0.06), nrow=7)
colnames(mat) <- c("Toughness Index", "Yield Strength", "Young's Modulus",
                  "Density", "Thermal Expansion", "Thermal Conductivity", "Specific Heat")
rownames(mat) <- c("AI 2024-T6", "AI 5052-O", "SS 301 FH",
                   "SS 310-3AH", "Ti-6AI-4V", "Inconel 718", "70Cu-30Zn")
weights <- c(0.28, 0.14, 0.05, 0.24, 0.19, 0.05, 0.05)
beneficial.vector <- c(1, 2, 3)
apply.CRADIS(mat, weights, beneficial.vector)



cleanEx()
nameEx("apply.CRITIC")
### * apply.CRITIC

flush(stderr()); flush(stdout())

### Name: apply.CRITIC
### Title: Apply CRITIC on comparison matrix
### Aliases: apply.CRITIC

### ** Examples

A <- matrix(c(250, 200, 300, 275,
 225, 16, 16, 32,
  32, 16, 12, 8,
   16, 8, 16, 5,
    3, 4, 4, 2), nrow=5, ncol=4)
colnames(A)<-c("Price", "Storage space", "Camera", "Looks")
rownames(A)<-paste0("Mobile ", seq(1, 5, 1))
A[,"Price"] <- -A[,"Price"]
apply.CRITIC(A)



cleanEx()
nameEx("apply.DEMATEL")
### * apply.DEMATEL

flush(stderr()); flush(stdout())

### Name: apply.DEMATEL
### Title: Apply DEMATEL method
### Aliases: apply.DEMATEL

### ** Examples

comparisons.mat <- matrix(c(0, 3, 3, 4,
1, 0, 2, 1,
1, 2, 0, 2,
1, 2, 1, 0), nrow=4)
rownames(comparisons.mat)<-c("Price/cost", "Storage Space", "Camera", "Processor")
colnames(comparisons.mat)<-c("Price/cost", "Storage Space", "Camera", "Processor")
apply.DEMATEL(comparisons.mat)



cleanEx()
nameEx("apply.EDAS")
### * apply.EDAS

flush(stderr()); flush(stdout())

### Name: apply.EDAS
### Title: Function to apply the Evaluation based on Distance from Average
###   Solution (EDAS) method
### Aliases: apply.EDAS

### ** Examples

mat <- matrix(c(250, 200, 300, 275, 225,
16, 16, 32, 32, 16,
12, 8, 16, 8, 16,
5, 3, 4, 4, 2), nrow=5)
colnames(mat)<-c("Price/cost", "Storage Space", "Camera", "Looks")
rownames(mat)<-paste0("Mobile", 1:5)
mat[,"Price/cost"]<--mat[,"Price/cost"]
weights <- c(0.35, 0.25, 0.25, 0.15)
apply.EDAS(mat, weights)



cleanEx()
nameEx("apply.ELECTRE1")
### * apply.ELECTRE1

flush(stderr()); flush(stdout())

### Name: apply.ELECTRE1
### Title: Apply ELECTRE I method
### Aliases: apply.ELECTRE1

### ** Examples

mat <- matrix(c(25, 10, 30, 20, 30, 10, 15, 20, 30, 30, 30, 10), nrow=3)
colnames(mat)<-c("c1", "c2", "c3", "c4")
rownames(mat)<-c("a1", "a2", "a3")
weights <- c(0.2, 0.15, 0.4, 0.25)

# Apply ELECTRE I method
results <- apply.ELECTRE1(mat, weights)




cleanEx()
nameEx("apply.FAHP")
### * apply.FAHP

flush(stderr()); flush(stdout())

### Name: apply.FAHP
### Title: Apply fuzzy AHP on criteria comparison matrix
### Aliases: apply.FAHP

### ** Examples

# example code
data <- read.csv(system.file("extdata", "AHP_input_file.csv", package = "RMCDA"), header=FALSE)
mat.lst <- read.csv.AHP.matrices(data)
mat.lst[[1]]->A
result <- apply.FAHP(A)



cleanEx()
nameEx("apply.GRA")
### * apply.GRA

flush(stderr()); flush(stdout())

### Name: apply.GRA
### Title: Apply Grey Relational Analysis (GRA) method
### Aliases: apply.GRA

### ** Examples

mat <- matrix(c(80, 60, 90,
                75, 85, 95,
                70, 65, 85,
                60, 75, 80),
              nrow = 4, byrow = TRUE)
colnames(mat) <- c("Criterion 1", "Criterion 2", "Criterion 3")
weights <- c(0.4, 0.3, 0.3)
beneficial.vector <- c(1, 2, 3)
apply.GRA(mat, weights, beneficial.vector)



cleanEx()
nameEx("apply.IDOCRIW")
### * apply.IDOCRIW

flush(stderr()); flush(stdout())

### Name: apply.IDOCRIW
### Title: Apply Integrated Determination of Objective Criteria Weights
###   (IDOCRIW) method
### Aliases: apply.IDOCRIW

### ** Examples

mat <- matrix(c(75.5, 95, 770, 187, 179, 239, 237,
420, 91, 1365, 1120, 875, 1190, 200,
74.2, 70, 189, 210, 112, 217, 112,
2.8, 2.68, 7.9, 7.9, 4.43, 8.51, 8.53,
21.4, 22.1, 16.9, 14.4, 9.4, 11.5, 19.9,
0.37, 0.33, 0.04, 0.03, 0.016, 0.31, 0.29,
0.16, 0.16, 0.08, 0.08, 0.09, 0.07, 0.06), nrow=7)
colnames(mat) <- c("Toughness Index", "Yield Strength", "Young's Modulus",
"Density", "Thermal Expansion", "Thermal Conductivity", "Specific Heat")
rownames(mat) <- c("AI 2024-T6", "AI 5052-O", "SS 301 FH",
"SS 310-3AH", "Ti-6AI-4V", "Inconel 718", "70Cu-30Zn")
beneficial.vector <- c(1, 2, 3, 6, 7)
apply.IDOCRIW(mat, beneficial.vector)



cleanEx()
nameEx("apply.MABAC")
### * apply.MABAC

flush(stderr()); flush(stdout())

### Name: apply.MABAC
### Title: Apply Multi-Attributive Border Approximation Area Comparison
###   (MABAC)
### Aliases: apply.MABAC

### ** Examples

# Example usage:
mat <- matrix(c(
  22600, 3800, 2,   5, 1.06, 3.00, 3.5,  2.8, 24.5, 6.5,
  19500, 4200, 3,   2, 0.95, 3.00, 3.4,  2.2, 24.0, 7.0,
  21700, 4000, 1,   3, 1.25, 3.20, 3.3,  2.5, 24.5, 7.3,
  20600, 3800, 2,   5, 1.05, 3.25, 3.2,  2.0, 22.5, 11.0,
  22500, 3800, 4,   3, 1.35, 3.20, 3.7,  2.1, 23.0, 6.3,
  23250, 4210, 3,   5, 1.45, 3.60, 3.5,  2.8, 23.5, 7.0,
  20300, 3850, 2,   5, 0.90, 3.25, 3.0,  2.6, 21.5, 6.0
), nrow = 7, byrow = TRUE)

weights <- c(0.146, 0.144, 0.119, 0.121, 0.115, 0.101, 0.088, 0.068, 0.050, 0.048)
types <- c(-1, 1, 1, 1, -1, -1, 1, 1, 1, 1)

apply.MABAC(mat, weights, types)



cleanEx()
nameEx("apply.MACBETH")
### * apply.MACBETH

flush(stderr()); flush(stdout())

### Name: apply.MACBETH
### Title: Apply MACBETH (Measuring Attractiveness by a Categorical Based
###   Evaluation TecHnique)
### Aliases: apply.MACBETH

### ** Examples

# Example matrix: 3 alternatives x 2 criteria
mat <- matrix(c(10, 5,
                12, 4,
                11, 6), nrow=3, byrow=TRUE)

# Suppose first column is beneficial, second is non-beneficial
benef.vec <- c(1)
wts <- c(0.6, 0.4)

# Get MACBETH scores
res <- apply.MACBETH(mat, benef.vec, wts)





cleanEx()
nameEx("apply.MAIRCA")
### * apply.MAIRCA

flush(stderr()); flush(stdout())

### Name: apply.MAIRCA
### Title: Apply Multi-Attributive Real Ideal Comparative Analysis (MAIRCA)
### Aliases: apply.MAIRCA

### ** Examples

# Example usage
mat <- matrix(c(70, 245, 16.4, 19,
                52, 246, 7.3, 22,
                53, 295, 10.3, 25,
                63, 256, 12.0, 8,
                64, 233, 5.3, 17),
              nrow = 5, byrow = TRUE)
weights <- c(0.04744, 0.02464, 0.51357, 0.41435)
types <- c(1, 1, 1, 1)
apply.MAIRCA(mat, weights, types)



cleanEx()
nameEx("apply.MARA")
### * apply.MARA

flush(stderr()); flush(stdout())

### Name: apply.MARA
### Title: Apply the MARA (Magnitude of the Area for the Ranking of
###   Alternatives) Method
### Aliases: apply.MARA

### ** Examples

# Example
mat <- matrix(c(10, 2,
                20, 4,
                15, 5),
              nrow = 3, byrow = TRUE)
weights <- c(0.7, 0.3)
beneficial.vector <- c(1)  # First column is beneficial (max); second is cost (min)
apply.MARA(mat, weights, beneficial.vector)




cleanEx()
nameEx("apply.MARCOS")
### * apply.MARCOS

flush(stderr()); flush(stdout())

### Name: apply.MARCOS
### Title: Apply Measurement of Alternatives and Ranking according to
###   Compromise Solution (MARCOS)
### Aliases: apply.MARCOS

### ** Examples


mat <- matrix(c(660, 1000, 1600, 18, 1200,
                800, 1000, 1600, 24, 900,
                980, 1000, 2500, 24, 900,
                920, 1500, 1600, 24, 900,
                1380, 1500, 1500, 24, 1150,
                1230, 1000, 1600, 24, 1150,
                680, 1500, 1600, 18, 1100,
                960, 2000, 1600, 12, 1150), nrow = 8, byrow = TRUE)
weights <- c(0.1061, 0.3476, 0.3330, 0.1185, 0.0949)
beneficial.vector <- c(2, 3, 4, 5)  # Columns 2, 3, 4, and 5 are beneficial
apply.MARCOS(mat, weights, beneficial.vector)



cleanEx()
nameEx("apply.MAUT")
### * apply.MAUT

flush(stderr()); flush(stdout())

### Name: apply.MAUT
### Title: Apply Multi-Attribute Utility Theory (MAUT) Method
### Aliases: apply.MAUT

### ** Examples

mat <- matrix(c(75.5, 95, 770, 187, 179, 239, 237, 420, 91), nrow = 3, byrow = TRUE)
weights <- c(0.3, 0.5, 0.2)
beneficial.vector <- c(1, 3)
utility.functions <- c("exp", "log", "quad")
step.size <- 1
result <- apply.MAUT(mat, weights, beneficial.vector, utility.functions, step.size)




cleanEx()
nameEx("apply.MOORA")
### * apply.MOORA

flush(stderr()); flush(stdout())

### Name: apply.MOORA
### Title: Apply Multi-Objective Optimization on the basis of Ratio
###   Analysis (MOORA)
### Aliases: apply.MOORA

### ** Examples

mat <- matrix(c(60, 6.35, 6.8, 10, 2.5, 4.5, 3,
0.4, 0.15, 0.1, 0.2, 0.1, 0.08, 0.1,
2540, 1016, 1727.2, 1000, 560, 1016, 177,
500, 3000, 1500, 2000, 500, 350, 1000,
990, 1041, 1676, 965, 915, 508, 920), nrow=7)
colnames(mat)<-c("Load capacity", "Repeatability", "Maximum tip speed",
"Memory capacity", "Manipulator reach")
rownames(mat)<-paste0("A", 1:7)
weights <- c(0.1574, 0.1825, 0.2385, 0.2172, 0.2043)
beneficial.vector <- c(1, 3, 4, 5)
apply.MOORA(mat, weights, beneficial.vector)



cleanEx()
nameEx("apply.MOOSRA")
### * apply.MOOSRA

flush(stderr()); flush(stdout())

### Name: apply.MOOSRA
### Title: Multi-objective Optimization on the Basis of Simple Ratio
###   Analysis (MOOSRA)
### Aliases: apply.MOOSRA

### ** Examples

mat <- matrix(c(75.5, 95, 770, 187, 179, 239, 237,
420, 91, 1365, 1120, 875, 1190, 200,
74.2, 70, 189, 210, 112, 217, 112,
2.8, 2.68, 7.9, 7.9, 4.43, 8.51, 8.53,
21.4, 22.1, 16.9, 14.4, 9.4, 11.5, 19.9,
0.37, 0.33, 0.04, 0.03, 0.016, 0.31, 0.29,
0.16, 0.16, 0.08, 0.08, 0.09, 0.07, 0.06), nrow=7)
weights <- c(0.1, 0.2, 0.3, 0.1, 0.1, 0.1, 0.1)
beneficial.vector<- c(1, 2, 3, 6, 7)
apply.MOOSRA(mat, weights, beneficial.vector)



cleanEx()
nameEx("apply.MULTIMOORA")
### * apply.MULTIMOORA

flush(stderr()); flush(stdout())

### Name: apply.MULTIMOORA
### Title: Apply MULTIMOORA method
### Aliases: apply.MULTIMOORA

### ** Examples

mat <- matrix(c(75.5, 95, 770, 187, 179, 239, 237,
                420, 91, 1365, 1120, 875, 1190, 200,
                74.2, 70, 189, 210, 112, 217, 112,
                2.8, 2.68, 7.9, 7.9, 4.43, 8.51, 8.53), nrow = 4, byrow = TRUE)
beneficial.vector <- c(1, 3) # Columns 1 and 3 are beneficial
apply.MULTIMOORA(mat, beneficial.vector)



cleanEx()
nameEx("apply.OCRA")
### * apply.OCRA

flush(stderr()); flush(stdout())

### Name: apply.OCRA
### Title: Apply Operational Competitiveness Rating (OCRA) method
### Aliases: apply.OCRA

### ** Examples

mat <- matrix(c(
  7.7, 256, 7.2, 7.3, 7.3,
  8.1, 250, 7.9, 7.8, 7.7,
  8.7, 352, 8.6, 7.9, 8.0,
  8.1, 262, 7.0, 8.1, 7.2,
  6.5, 271, 6.3, 6.4, 6.1,
  6.8, 228, 7.1, 7.2, 6.5
), nrow = 6, byrow = TRUE)

weights <- c(0.239, 0.225, 0.197, 0.186, 0.153)
beneficial.vector <- c(1, 3, 4, 5)

apply.OCRA(mat, weights, beneficial.vector)



cleanEx()
nameEx("apply.OPA")
### * apply.OPA

flush(stderr()); flush(stdout())

### Name: apply.OPA
### Title: Apply Ordinal Priority Approach (OPA)
### Aliases: apply.OPA

### ** Examples

# Input Data
expert.x.alt <- matrix(c(1, 3, 2, 2, 1, 3), nrow = 3)
colnames(expert.x.alt) <- c("c", "q")
rownames(expert.x.alt) <- c("alt1", "alt2", "alt3")

expert.y.alt <- matrix(c(1, 2, 3, 3, 1, 2), nrow = 3)
colnames(expert.y.alt) <- c("c", "q")
rownames(expert.y.alt) <- c("alt1", "alt2", "alt3")

expert.opinion.lst <- list(expert.x.alt, expert.y.alt)
expert.rank <- c(1, 2)  # Ranks of experts

# Criterion ranks for each expert
criterion.x.rank <- c(1, 2)
criterion.y.rank <- c(2, 1)  # Adjusted criterion rank for expert y
criterion.rank.lst <- list(criterion.x.rank, criterion.y.rank)

# Apply OPA
weights <- apply.OPA(expert.opinion.lst, expert.rank, criterion.rank.lst)




cleanEx()
nameEx("apply.ORESTE")
### * apply.ORESTE

flush(stderr()); flush(stdout())

### Name: apply.ORESTE
### Title: Apply the ORESTE (Organisation Rangement Et SynThèsE de données
###   relationnelles) Method
### Aliases: apply.ORESTE

### ** Examples

mat <- matrix(c(10, 2,
                20, 4,
                15, 5),
              nrow = 3, byrow = TRUE)
weights <- c(0.7, 0.3)
beneficial.vector <- c(1)   # 1st column "max", 2nd column "min"

apply.ORESTE(mat, weights, beneficial.vector, alpha = 0.4)




cleanEx()
nameEx("apply.PIV")
### * apply.PIV

flush(stderr()); flush(stdout())

### Name: apply.PIV
### Title: Apply Proximity Indexed Value (PIV) method
### Aliases: apply.PIV

### ** Examples

mat <- matrix(c(80, 60, 90,
                75, 85, 95,
                70, 65, 85,
                60, 75, 80),
              nrow = 4, byrow = TRUE)
colnames(mat) <- c("Criterion 1", "Criterion 2", "Criterion 3")
weights <- c(0.4, 0.3, 0.3)
beneficial.vector <- c(1, 2, 3)
apply.PIV(mat, weights, beneficial.vector)



cleanEx()
nameEx("apply.PROMETHEE")
### * apply.PROMETHEE

flush(stderr()); flush(stdout())

### Name: apply.PROMETHEE
### Title: Function for applying PROMOTHEE I or II
### Aliases: apply.PROMETHEE

### ** Examples

A <- matrix(c(250, 200, 300, 275, 16, 16, 32, 32, 12, 8, 16, 8, 5, 3, 4, 2), nrow=4)
rownames(A)<-c("Mobile 1", "Mobile 2", "Mobile 3", "Mobile 4")
colnames(A)<-c("Price", "Memory", "Camera", "Looks")
weights <- c(0.35, 0.25, 0.25, 0.15)
apply.PROMETHEE(A, weights)



cleanEx()
nameEx("apply.PSI")
### * apply.PSI

flush(stderr()); flush(stdout())

### Name: apply.PSI
### Title: Apply Preference Selection Index (PSI) method
### Aliases: apply.PSI

### ** Examples

mat <- matrix(c(80, 60, 90,
                75, 85, 95,
                70, 65, 85,
                60, 75, 80),
              nrow = 4, byrow = TRUE)
colnames(mat) <- c("Criterion 1", "Criterion 2", "Criterion 3")
beneficial.vector <- c(1, 2, 3)
apply.PSI(mat, beneficial.vector)



cleanEx()
nameEx("apply.RAFSI")
### * apply.RAFSI

flush(stderr()); flush(stdout())

### Name: apply.RAFSI
### Title: Ranking of Alternatives through Functional mapping of criterion
###   sub-intervals into a Single Interval (RAFSI)
### Aliases: apply.RAFSI

### ** Examples

mat <- matrix(c(3, 2, 5,
4, 3, 2,
1, 6, 4),
nrow = 3, byrow = TRUE)
weights <- c(0.3, 0.5, 0.2)
beneficial.vector <- c(1, 2)
apply.RAFSI(mat, weights, beneficial.vector,   n_i = 1, n_k = 6)



cleanEx()
nameEx("apply.REGIME")
### * apply.REGIME

flush(stderr()); flush(stdout())

### Name: apply.REGIME
### Title: Apply REGIME method (using a beneficial.vector)
### Aliases: apply.REGIME

### ** Examples

# Example data: 3 alternatives x 2 criteria
mat <- matrix(c(10, 5,
                12, 4,
                11, 6), nrow = 3, byrow = TRUE)

# Suppose first column is beneficial, second is non-beneficial
benef.vec <- c(1)  # means col1 is "max", col2 is "min"
wts <- c(0.6, 0.4)

# Call apply.REGIME without partial-order
regime.out <- apply.REGIME(mat, benef.vec, wts, doPreOrder = FALSE)


# Or with partial-order
regime.out2 <- apply.REGIME(mat, benef.vec, wts, doPreOrder = TRUE)




cleanEx()
nameEx("apply.RIM")
### * apply.RIM

flush(stderr()); flush(stdout())

### Name: apply.RIM
### Title: Function to apply Reference Ideal Method (RIM) Note: function is
###   rewritten from the MCDM package to match the formatting of the R
###   RMCDA package SOURCE:
###   https://github.com/cran/MCDM/blob/master/R/RIM.R
### Aliases: apply.RIM

### ** Examples


# Example decision matrix
mat <- matrix(
  c(30,40,25,27,45,0,
    9,0,0,15,2,1,
    3,5,2,3,3,1,
    3,2,3,3,3,2,
    2,2,1,4,1,2),
  nrow = 5, ncol = 6, byrow = TRUE
)

#Example weights vector (must sum to 1)
weights <- c(0.2262,0.2143,0.1786,0.1429,0.119,0.119)

#Example AB matrix
AB <- matrix(
  c(23,60,0,15,0,10,
    1,3,1,3,1,5),
  nrow = 2, ncol = 6, byrow = TRUE
)

#Example CD matrix
CD <- matrix(
  c(30,35,10,15,0,0,
    3,3,3,3,4,5),
  nrow = 2, ncol = 6, byrow = TRUE
)


apply.RIM(mat, weights, AB, CD)




cleanEx()
nameEx("apply.ROV")
### * apply.ROV

flush(stderr()); flush(stdout())

### Name: apply.ROV
### Title: Apply Range of Value (ROV) method
### Aliases: apply.ROV

### ** Examples

mat <- matrix(c(80, 60, 90,
                75, 85, 95,
                70, 65, 85,
                60, 75, 80),
              nrow = 4, byrow = TRUE)
colnames(mat) <- c("Criterion 1", "Criterion 2", "Criterion 3")
weights <- c(0.4, 0.3, 0.3)
beneficial.vector <- c(1, 2, 3)
apply.ROV(mat, weights, beneficial.vector)



cleanEx()
nameEx("apply.SAW")
### * apply.SAW

flush(stderr()); flush(stdout())

### Name: apply.SAW
### Title: Apply Simple Additive Weighting Method (SAW)
### Aliases: apply.SAW

### ** Examples

mat <- matrix(c(60, 6.35, 6.8, 10, 2.5, 4.5, 3,
0.4, 0.15, 0.1, 0.2, 0.1, 0.08, 0.1,
2540, 1016, 1727.2, 1000, 560, 1016, 177,
500, 3000, 1500, 2000, 500, 350, 1000,
990, 1041, 1676, 965, 915, 508, 920), nrow=7)
colnames(mat)<-c("Load capacity", "Repeatability", "Maximum tip speed",
"Memory capacity", "Manipulator reach")
rownames(mat)<-paste0("A", 1:7)
weights <- c(0.1574, 0.1825, 0.2385, 0.2172, 0.2043)
beneficial.vector <- c(1, 3, 4, 5)
apply.SAW(mat, weights, beneficial.vector)



cleanEx()
nameEx("apply.SBWM")
### * apply.SBWM

flush(stderr()); flush(stdout())

### Name: apply.SBWM
### Title: Function for applying the Stratified Best-Worst Method (SBWM)
### Aliases: apply.SBWM

### ** Examples

data <- read.csv(system.file("extdata",
 "stratified_BWM_case_study_I_example.csv",
  package = "RMCDA"), header = FALSE)
mat.lst <- read.csv.SBWM.matrices(data)
comparison.mat <- mat.lst[[1]]
others.to.worst <- mat.lst[[2]]
others.to.best <- mat.lst[[3]]
state.worst.lst <- mat.lst[[4]]
state.best.lst <- mat.lst[[5]]
likelihood.vector <- mat.lst[[6]]
apply.SBWM(comparison.mat, others.to.worst,
 others.to.best, state.worst.lst,
  state.best.lst, likelihood.vector)



cleanEx()
nameEx("apply.SECA")
### * apply.SECA

flush(stderr()); flush(stdout())

### Name: apply.SECA
### Title: Apply Simultaneous Evaluation of Criteria and Alternatives
###   (SECA) method
### Aliases: apply.SECA

### ** Examples

mat <- matrix(c(80, 60, 90,
                75, 85, 95,
                70, 65, 85,
                60, 75, 80),
              nrow = 4, byrow = TRUE)
colnames(mat) <- c("Criterion 1", "Criterion 2", "Criterion 3")
beneficial.vector <- c(1, 2, 3)
apply.SECA(mat, beneficial.vector)



cleanEx()
nameEx("apply.SMART")
### * apply.SMART

flush(stderr()); flush(stdout())

### Name: apply.SMART
### Title: Apply the SMART Method
### Aliases: apply.SMART

### ** Examples

# Example usage
data_mat <- matrix(c(10, 20, 15,  7,
                     30,  5,  8, 25),
                   nrow = 2, byrow = TRUE)
# Suppose we have 4 criteria (2 rows, 4 columns)
# We'll treat columns 1, 2, 3 as beneficial, and column 4 as non-beneficial
benef_vec <- c(1, 2, 3)

# Grades for each of 4 criteria
grades <- c(2, 2, 1, 3)
lower  <- c(0, 0, 0,  0)
upper  <- c(40, 40, 40, 40)

# Run SMART
result <- apply.SMART(dataset = data_mat,
                    grades = grades,
                    lower  = lower,
                    upper  = upper,
                    beneficial.vector = benef_vec)

result




cleanEx()
nameEx("apply.SMCDM")
### * apply.SMCDM

flush(stderr()); flush(stdout())

### Name: apply.SMCDM
### Title: Apply Stratified Multi-Criteria Decision Making (SMCDM) method
### Aliases: apply.SMCDM

### ** Examples

data <- read.csv(system.file("extdata", "SMCDM_input.csv", package = "RMCDA"), header=FALSE)
mat.lst <- read.csv.SMCDM.matrices(data)
comparison.mat <- mat.lst[[1]]
state.criteria.probs <- mat.lst[[2]]
likelihood.vector <- mat.lst[[3]]
apply.SMCDM(comparison.mat, state.criteria.probs, likelihood.vector)



cleanEx()
nameEx("apply.SPOTIS")
### * apply.SPOTIS

flush(stderr()); flush(stdout())

### Name: apply.SPOTIS
### Title: Apply the Stable Preference Ordering Towards Ideal Solution
###   (SPOTIS) method
### Aliases: apply.SPOTIS

### ** Examples

# Decision matrix
matrix <- matrix(c(10.5, -3.1, 1.7,
                   -4.7, 0, 3.4,
                   8.1, 0.3, 1.3,
                   3.2, 7.3, -5.3), nrow = 4, byrow = TRUE)

# Criteria bounds
bounds <- matrix(c(-5, 12,
                   -6, 10,
                   -8, 5), nrow = 3, byrow = TRUE)

# Criteria weights
weights <- c(0.2, 0.3, 0.5)

# Criteria types
types <- c(1, -1, 1)

# Apply SPOTIS
preferences <- apply.SPOTIS(matrix, weights, types, bounds)




cleanEx()
nameEx("apply.SRMP")
### * apply.SRMP

flush(stderr()); flush(stdout())

### Name: apply.SRMP
### Title: Apply SRMP (Simple Ranking Method using Reference Profiles) on
###   data
### Aliases: apply.SRMP

### ** Examples

evaluations.mat <- matrix(c(41, 46, 43, -2, -4, -5.5, 4, 2, 3), nrow=3)
colnames(evaluations.mat) <- c("S", "L", "J")
rownames(evaluations.mat) <- c("x", "y", "z")
reference.profiles <- matrix(c(42, 45, -5, -3, 2, 4), nrow=2)
colnames(reference.profiles) <- c("S", "L", "J")
rownames(reference.profiles) <- c("p1", "p2")
weights <- c(1/3, 1/3, 1/3)
apply.SRMP(evaluations.mat, reference.profiles, weights)



cleanEx()
nameEx("apply.TODIM")
### * apply.TODIM

flush(stderr()); flush(stdout())

### Name: apply.TODIM
### Title: Apply TODIM (TOmada de Decisao Interativa e Multicriterio)
### Aliases: apply.TODIM

### ** Examples

# Small synthetic example
mat <- matrix(c(75.5, 95, 770, 187, 179, 239, 237,
420, 91, 1365, 1120, 875, 1190, 200,
74.2, 70, 189, 210, 112, 217, 112,
2.8, 2.68, 7.9, 7.9, 4.43, 8.51, 8.53,
21.4, 22.1, 16.9, 14.4, 9.4, 11.5, 19.9,
0.37, 0.33, 0.04, 0.03, 0.016, 0.31, 0.29,
0.16, 0.16, 0.08, 0.08, 0.09, 0.07, 0.06), nrow=7)

colnames(mat)<-c("Toughness Index",	"Yield Strength",	"Young's Modulus",
"Density",	"Thermal Expansion",	"Thermal Conductivity","Specific Heat")
rownames(mat)<-c("AI 2024-T6", "AI 5052-O","SS 301 FH",
"SS 310-3AH","Ti-6AI-4V","Inconel 718","70Cu-30Zn")
weights <- c(0.28, 0.14, 0.05, 0.24, 0.19, 0.05, 0.05)
beneficial.vector<-c(1,2,3)

apply.TODIM(mat, weights, beneficial.vector, teta=1)




cleanEx()
nameEx("apply.TOPSIS")
### * apply.TOPSIS

flush(stderr()); flush(stdout())

### Name: apply.TOPSIS
### Title: Apply TOPSIS on matrix A with weight of criteria stored in
###   vector w
### Aliases: apply.TOPSIS

### ** Examples

A <- matrix(c(250, 200, 300, 275,
 225, 16, 16, 32,
  32, 16, 12, 8,
   16, 8, 16, 5,
    3, 4, 4, 2), nrow=5, ncol=4)
colnames(A)<-c("Price", "Storage space",
 "Camera", "Looks")
rownames(A)<-paste0("Mobile ", seq(1, 5, 1))
A[,"Price"] <- -A[,"Price"]
apply.TOPSIS(A, c(1/4, 1/4, 1/4, 1/4))



cleanEx()
nameEx("apply.VIKOR")
### * apply.VIKOR

flush(stderr()); flush(stdout())

### Name: apply.VIKOR
### Title: Function for applying VIKOR to data
### Aliases: apply.VIKOR

### ** Examples

A <- matrix(c(250, 200, 300, 275,
 225, 16, 16, 32,
  32, 16, 12, 8,
   16, 8, 16, 5,
    3, 4, 4, 2), nrow=5, ncol=4)
colnames(A)<-c("Price", "Memory", "Camera", "Looks")
rownames(A)<-paste0("Mobile ", seq(1, 5, 1))
A[,"Price"] <- -A[,"Price"]
apply.VIKOR(A, c(0.35, 0.3, 0.2, 0.15))



cleanEx()
nameEx("apply.WASPAS")
### * apply.WASPAS

flush(stderr()); flush(stdout())

### Name: apply.WASPAS
### Title: Weighted Aggregated Sum Product Assessment (WASPAS)
### Aliases: apply.WASPAS

### ** Examples

mat <- matrix(c(0.04, 0.11, 0.05, 0.02, 0.08, 0.05, 0.03, 0.1, 0.03,
1.137, 0.854, 1.07, 0.524, 0.596, 0.722, 0.521, 0.418, 0.62,
960, 1920, 3200, 1280, 2400, 1920, 1600, 1440, 2560), nrow=9)
colnames(mat)<-c("Dimensional Deviation (DD)", "Surface Roughness (SR)",
"Material Removal Rate (MRR)")

rownames(mat)<-paste0("A", 1:9)
beneficial.vector <- c(3)
weights <- c(0.1047, 0.2583, 0.6369)
apply.WASPAS(mat, weights, beneficial.vector, 0.5)



cleanEx()
nameEx("apply.WINGS")
### * apply.WINGS

flush(stderr()); flush(stdout())

### Name: apply.WINGS
### Title: Apply WINGS (Weighted Influence Non-linear Gauge System)
### Aliases: apply.WINGS

### ** Examples


mat <- matrix(c(75.5, 95, 770, 187, 179, 239, 237,
420, 91, 1365, 1120, 875, 1190, 200,
74.2, 70, 189, 210, 112, 217, 112,
2.8, 2.68, 7.9, 7.9, 4.43, 8.51, 8.53,
21.4, 22.1, 16.9, 14.4, 9.4, 11.5, 19.9,
0.37, 0.33, 0.04, 0.03, 0.016, 0.31, 0.29,
0.16, 0.16, 0.08, 0.08, 0.09, 0.07, 0.06), nrow=7)

colnames(mat)<-c("Toughness Index",	"Yield Strength",	"Young's Modulus",
"Density",	"Thermal Expansion",	"Thermal Conductivity","Specific Heat")
rownames(mat)<-c("AI 2024-T6", "AI 5052-O","SS 301 FH",
"SS 310-3AH","Ti-6AI-4V","Inconel 718","70Cu-30Zn")

result <- apply.WINGS(mat)
result$r_plus_c    # (R + C)
result$r_minus_c   # (R - C)
result$weights     # Weights



cleanEx()
nameEx("apply.WISP")
### * apply.WISP

flush(stderr()); flush(stdout())

### Name: apply.WISP
### Title: Apply WISP (Integrated Simple Weighted Sum Product) method,
### Aliases: apply.WISP

### ** Examples

mat <- matrix(c(75.5, 95, 770, 187, 179, 239, 237,
420, 91, 1365, 1120, 875, 1190, 200,
74.2, 70, 189, 210, 112, 217, 112,
2.8, 2.68, 7.9, 7.9, 4.43, 8.51, 8.53,
21.4, 22.1, 16.9, 14.4, 9.4, 11.5, 19.9,
0.37, 0.33, 0.04, 0.03, 0.016, 0.31, 0.29,
0.16, 0.16, 0.08, 0.08, 0.09, 0.07, 0.06), nrow=7)

colnames(mat)<-c("Toughness Index",	"Yield Strength",	"Young's Modulus",
"Density",	"Thermal Expansion",	"Thermal Conductivity","Specific Heat")
rownames(mat)<-c("AI 2024-T6", "AI 5052-O","SS 301 FH",
"SS 310-3AH","Ti-6AI-4V","Inconel 718","70Cu-30Zn")

# Suppose the first two columns are beneficial, and the 3rd is non-beneficial
beneficial.vector <- c(1,2, 4)
weights <- c(0.28, 0.14, 0.05, 0.24, 0.19, 0.05, 0.05)

# Get the WISP scores
apply.WISP(mat, beneficial.vector, weights, simplified=FALSE)




cleanEx()
nameEx("apply.WSM_WPM")
### * apply.WSM_WPM

flush(stderr()); flush(stdout())

### Name: apply.WSM_WPM
### Title: Apply Weighted Sum Model (WSM) or Weighted Product Model (WPM)
###   on data
### Aliases: apply.WSM_WPM

### ** Examples

mat <- matrix(c(250, 200, 300, 275,
 225, 16, 16, 32,
  32, 16, 12, 8,
   16, 8, 16, 5,
    3, 4, 4, 2), nrow=5, ncol=4)
colnames(mat)<-c("Price", "Storage space",
 "Camera", "Looks")
rownames(mat)<-paste0("Mobile ", seq(1, 5, 1))
beneficial.vector <- c(2, 3, 4)
weights <- c(0.25, 0.25, 0.25, 0.25)
apply.WSM_WPM(mat, beneficial.vector, weights, "WSM")




cleanEx()
nameEx("apply.entropy")
### * apply.entropy

flush(stderr()); flush(stdout())

### Name: apply.entropy
### Title: Find entropy of each criteria
### Aliases: apply.entropy

### ** Examples

A <- matrix(c(250, 200, 300, 275,
 225, 16, 16, 32,
  32, 16, 12, 8,
   16, 8, 16, 5,
    3, 4, 4, 2), nrow=5, ncol=4)
colnames(A)<-c("Price", "Storage space",
 "Camera", "Looks")
rownames(A)<-paste0("Mobile ", seq(1, 5, 1))
A[,"Price"] <- -A[,"Price"]
apply.entropy(A)



cleanEx()
nameEx("apply.po.ranking")
### * apply.po.ranking

flush(stderr()); flush(stdout())

### Name: apply.po.ranking
### Title: Apply Pre-Order Ranking (partial-order analysis)
### Aliases: apply.po.ranking

### ** Examples

# Create a small 3x3 partial-order matrix
po_str <- matrix(c("P+", "P+", "R",
                   "R",   "-",   "I",
                   "R",   "I",   "-"), nrow=3, byrow=TRUE)

# Apply the pre-order ranking
res <- apply.po.ranking(po_str)





cleanEx()
nameEx("generate.SPOTIS.bounds")
### * generate.SPOTIS.bounds

flush(stderr()); flush(stdout())

### Name: generate.SPOTIS.bounds
### Title: Generate bounds for criteria from a decision matrix
### Aliases: generate.SPOTIS.bounds

### ** Examples

# Decision matrix
matrix <- matrix(c(96, 145, 200,
                   100, 145, 200,
                   120, 170, 80,
                   140, 180, 140,
                   100, 110, 30), nrow = 5, byrow = TRUE)

# Generate bounds
bounds <- generate.SPOTIS.bounds(matrix)




cleanEx()
nameEx("read.csv.AHP.matrices")
### * read.csv.AHP.matrices

flush(stderr()); flush(stdout())

### Name: read.csv.AHP.matrices
### Title: Read csv file containing pairwise comparison matrices for
###   applying AHP or ANP
### Aliases: read.csv.AHP.matrices

### ** Examples

data <- read.csv(system.file("extdata", "AHP_input_file.csv",
 package = "RMCDA"), header=FALSE)
mat.lst <- read.csv.AHP.matrices(data)



cleanEx()
nameEx("read.csv.SBWM.matrices")
### * read.csv.SBWM.matrices

flush(stderr()); flush(stdout())

### Name: read.csv.SBWM.matrices
### Title: Read csv file containing input to the stratified BWM method
### Aliases: read.csv.SBWM.matrices

### ** Examples

data <- read.csv(system.file("extdata",
"stratified_BWM_case_study_I_example.csv",
 package = "RMCDA"), header = FALSE)
mat.lst <- read.csv.SBWM.matrices(data)



cleanEx()
nameEx("read.csv.SMCDM.matrices")
### * read.csv.SMCDM.matrices

flush(stderr()); flush(stdout())

### Name: read.csv.SMCDM.matrices
### Title: Read csv file containing pairwise comparison matrices for
###   applying SMCDM
### Aliases: read.csv.SMCDM.matrices

### ** Examples

data <- read.csv(system.file("extdata", "SMCDM_input.csv", package = "RMCDA"), header = FALSE)
mat.lst <- read.csv.SMCDM.matrices(data)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
