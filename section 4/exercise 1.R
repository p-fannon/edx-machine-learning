library(dslabs)
data(tissue_gene_expression)

dim(tissue_gene_expression$x)

table(tissue_gene_expression$y)

# computes the Euclidean distance between each observation and stores it in the object d
d <- dist(tissue_gene_expression$x)

image(as.matrix(d))
