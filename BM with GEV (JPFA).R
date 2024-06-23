library(readxl)
Cycle=read_excel("Return.xlsx")

######BLOCK MAXIMA---##########
# Fungsi untuk mencari blok maksimum dan menyimpannya dalam data1frame
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
JPFA_df = data.frame(JPFA = find_block_maxima(Cycle$JPFA))
Combined=cbind(HMSP_df,JPFA_df)

##########GEV dengan mencari moment dengan manual
###1. Untuk data1 HMSP

# untuk l_1
l_1 <- function(data1) {
  n <- length(data1)
  data1 <- sort(data1)
  hasil <- 0
  for (i in 1:n) {
    hasil <- hasil + data1[i]/n
  }
  return(hasil)
}

# Untuk l_2
l_2 <- function(data1) {
  n <- length(data1)
  data1 <- sort(data1)  #Mengurutukan data1
  hasil <- 0  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      hasil <- hasil + (data1[j] - data1[i])
    }
  }
  hasil <- hasil / (2 * choose(n, 2)) 
  return(hasil)
}

# Untuk l_3
l_3 <- function(data1) {
  n <- length(data1)
  data1 <- sort(data1)  # Mengurutkan data1
  hasil <- 0  
  for (i in 1:(n-2)) {
    for (j in (i+1):(n-1)) {
      for (k in (j+1):n) {
        hasil <- hasil + (data1[k] - 2 * data1[j] + data1[i])
      }
    }
  }
  hasil <- hasil / (3 * choose(n, 3)) 
  return(hasil)
}

#untuk l_4
# Fungsi untuk menghitung persamaan l_4
l_4 <- function(data1) {
  n <- length(data1)
  data1 <- sort(data1)  # Mengurutkan data1
  hasil <- 0  
  for (i in 1:(n-3)) {
    for (j in (i+1):(n-2)) {
      for (k in (j+1):(n-1)) {
        for (l in (k+1):n) {
          hasil <- hasil + (data1[l] - 3 * data1[k] + 3 * data1[j] - data1[i])
        }
      }
    }
  }
  hasil <- hasil / (4 * choose(n, 4)) 
  return(hasil)
}

## Untuk data HMSP
data1 <- JPFA_df$JPFA 
#l_1
l_1<-l_1(data1)
print(l_1)
#l_2
l_2<-l_2(data1)
print(l_2)
#l_3
l_3 <- l_3(data)
print(l_3)
#l_4
l_4=l_4(data1)
print(l_4)

#tau_3
tau_3=l_3/l_2

#tau_4
tau_4=l_4/l_2

##GEV untuk data HMSP
LM2=data.frame(l_1=l_1,l_2=l_2,l_3=l_3,
               l_4=l_4,tau_3=tau_3,tau_4=tau_4)
LM2