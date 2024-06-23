### Tanpa package (Hosking approach with -xi)
gev_pdf <- function(x, mu, sigma, xi) {
  if (xi != 0) {
    return ((1/sigma) * (1 - xi * ((x - mu) / sigma))^((1/xi) - 1) * exp(-((1 - xi * ((x - mu) / sigma))^(1/xi))))
  } else {
    return ((1/sigma) * exp(-((x - mu) / sigma)) * exp(-exp(-((x - mu) / sigma))))
  }
}

# Perkiraan nilai x
x <- seq(-6,10, length.out = 1000)

# Parameters
mu <- 0
sigma <- 1
xi_values <- c(-0.5, 0, 0.5)

# Compute the GEV PDF for different xi values
pdfs_gev <- sapply(xi_values, function(xi) sapply(x, gev_pdf, mu = mu, sigma = sigma, xi = xi))

# Set up plot colors
colors <- c("red", "blue", "green")

# Plot the PDF for each xi value
plot(x, pdfs_gev[,1], type="l", col=colors[1], lwd=2, ylim=c(0, max(pdfs_gev)), ylab="Probability Density Function of GEV Distribution", xlab="", main="")
for (i in 2:length(xi_values)) {
  lines(x, pdfs_gev[,i], col=colors[i], lwd=2)
}

# Add legend
legend("topright", legend=c(xi<0~",Frechet", xi==0~",Gumbel", xi>0~",Weibull"), col=colors, lty=1,lwd=2)