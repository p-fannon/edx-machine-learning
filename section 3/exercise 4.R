# compute the proportion of pixels that are in the grey area, defined as values between 50 and 205 (but not including 50 and 205)
mnist <- read_mnist()
y <- rowMeans(mnist$train$images>50 & mnist$train$images<205)
qplot(as.factor(mnist$train$labels), y, geom = "boxplot")
mean(y) # proportion of pixels
