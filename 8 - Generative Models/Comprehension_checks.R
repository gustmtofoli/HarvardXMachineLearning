library(caret)

# Q1 ===============================================================
set.seed(1993)
data("tissue_gene_expression")

ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
mod <- train(x,y ,method="lda")
mod
mod$finalModel$means
# ==================================================================

# Q3 ===============================================================
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
mod <- train(x,y ,method="qda")
mod
mod$finalModel$means

# ===================================================================

# Q5 ================================================================
set.seed(1993)
data("tissue_gene_expression")

ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
mod <- train(x,y ,method="lda", preProcessing = "scale")
mod
mod$finalModel$means
# ==================================================================


# Q6 ===============================================================

set.seed(1993)
data("tissue_gene_expression")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]
mod <- train(x,y ,method="lda")
mod
# ==================================================================