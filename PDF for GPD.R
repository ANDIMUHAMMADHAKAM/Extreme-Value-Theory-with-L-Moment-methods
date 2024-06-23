### Tanpa package (Hosking approach with -xi)
gpd_pdf_modified <- function(x, mu, sigma, xi) {
  if (xi != 0) {
    return ((1/sigma) * (1 - (xi * (x - mu)) / sigma)^(1/xi - 1))
  } else {
    return ((1/sigma) * exp(-(x - mu) / sigma))
  }
}

# Perkiraan nilai x
x <- seq(2,10, length.out = 1000)

# Parameter
mu <- 0.3
sigma <- 1
xi_values <- c(-0.1, 0, 0.1)

# Compute the modified GPD PDF for different xi values
pdfs_modified <- sapply(xi_values, function(xi) sapply(x, gpd_pdf_modified, mu = mu, sigma = sigma, xi = xi))

# Set up plot colors
colors <- c("red", "blue", "green")

# Plot the modified PDF for each xi value
plot(x, pdfs_modified[,1], type="l", col=colors[1], lwd=2, ylim=c(0, max(pdfs_modified)), ylab="Probability Density Function of GPD", xlab="x")
for (i in 2:length(xi_values)) {
  lines(x, pdfs_modified[,i], col=colors[i], lwd=2)
}

legend("topright", legend=c(xi<0~",Pareto", xi==0~",Eksponensial", xi>0~",Beta"), col=colors, lty=1,lwd=2)