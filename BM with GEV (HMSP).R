library(readxl)
Cycle=read_excel("Return.xlsx")

######BLOCK MAXIMA---##########
# Fungsi untuk mencari blok maksimum dan menyimpannya dalam dataframe
find_block_maxima <- function(stock_column, stock_name) {
  max_values <- numeric(length = length(stock_column) %/% 5)
  for (i in 1:(length(stock_column) %/% 5)) {
    maximum <- stock_column[(i - 1) * 5 + 1]
    for (j in 2:5) {
      if (stock_column[(i - 1) * 5 + j] > maximum) {
        maximum <- stock_column[(i - 1) * 5 + j]
      }
    }
    max_values[i] <- maximum
  }
  return(max_values)
}
HMSP_df <- data.frame(HMSP = find_block_maxima(Cycle$HMSP))

##########GEV dengan mencari moment dengan manual
###1. Untuk data HMSP

# untuk l_1
l_1 <- function(data) {
  n <- length(data)
  data <- sort(data)
  hasil <- 0
  for (i in 1:n) {
    hasil <- hasil + data[i]/n
  }
  return(hasil)
}

# Untuk l_2
l_2 <- function(data) {
  n <- length(data)
  data <- sort(data)  #Mengurutukan data
  hasil <- 0  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      hasil <- hasil + (data[j] - data[i])
    }
  }
  hasil <- hasil / (2 * choose(n, 2)) 
  return(hasil)
}

# Untuk l_3
l_3 <- function(data) {
  n <- length(data)
  data <- sort(data)  # Mengurutkan data
  hasil <- 0  
  for (i in 1:(n-2)) {
    for (j in (i+1):(n-1)) {
      for (k in (j+1):n) {
        hasil <- hasil + (data[k] - 2 * data[j] + data[i])
      }
    }
  }
  hasil <- hasil / (3 * choose(n, 3)) 
  return(hasil)
}

#untuk l_4
# Fungsi untuk menghitung persamaan l_4
l_4 <- function(data) {
  n <- length(data)
  data <- sort(data)  # Mengurutkan data
  hasil <- 0  
  for (i in 1:(n-3)) {
    for (j in (i+1):(n-2)) {
      for (k in (j+1):(n-1)) {
        for (l in (k+1):n) {
          hasil <- hasil + (data[l] - 3 * data[k] + 3 * data[j] - data[i])
        }
      }
    }
  }
  hasil <- hasil / (4 * choose(n, 4)) 
  return(hasil)
}

## Untuk data HMSP
data <- HMSP_df$HMSP
#l_1
l_1<-l_1(data)
print(l_1)
#l_2
l_2<-l_2(data)
print(l_2)
#l_3
l_3 <- l_3(data)
print(l_3)
#l_4
l_4=l_4(data)
print(l_4)

#tau_3
tau_3=l_3/l_2

#tau_4
tau_4=l_4/l_2

##GEV untuk data HMSP
LM1=data.frame(l_1=l_1,l_2=l_2,l_3=l_3,
               l_4=l_4,tau_3=tau_3,tau_4=tau_4)
LM1