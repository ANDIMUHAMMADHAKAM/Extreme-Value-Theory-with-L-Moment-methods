library(readxl)
POT=read_excel("POT.xlsx")


# untuk l_1
l_1 <- function(data2) {
  n <- length(data2)
  data2 <- sort(data2)
  hasil <- 0
  for (i in 1:n) {
    hasil <- hasil + data2[i]/n
  }
  return(hasil)
}

# Untuk l_2
l_2 <- function(data2) {
  n <- length(data2)
  data2 <- sort(data2)  #Mengurutukan data2
  hasil <- 0  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      hasil <- hasil + (data2[j] - data2[i])
    }
  }
  hasil <- hasil / (2 * choose(n, 2)) 
  return(hasil)
}

# Untuk l_3
l_3 <- function(data2) {
  n <- length(data2)
  data2 <- sort(data2)  # Mengurutkan data2
  hasil <- 0  
  for (i in 1:(n-2)) {
    for (j in (i+1):(n-1)) {
      for (k in (j+1):n) {
        hasil <- hasil + (data2[k] - 2 * data2[j] + data2[i])
      }
    }
  }
  hasil <- hasil / (3 * choose(n, 3)) 
  return(hasil)
}

#untuk l_4
# Fungsi untuk menghitung persamaan l_4
l_4 <- function(data2) {
  n <- length(data2)
  data2 <- sort(data2)  # Mengurutkan data2
  hasil <- 0  
  for (i in 1:(n-3)) {
    for (j in (i+1):(n-2)) {
      for (k in (j+1):(n-1)) {
        for (l in (k+1):n) {
          hasil <- hasil + (data2[l] - 3 * data2[k] + 3 * data2[j] - data2[i])
        }
      }
    }
  }
  hasil <- hasil / (4 * choose(n, 4)) 
  return(hasil)
}

## Untuk data2 HMSP
data2 <- POT$HMSP
#l_1
l_1<-l_1(data2)
print(l_1)
#l_2
l_2<-l_2(data2)
print(l_2)
#l_3
l_3 <- l_3(data2)
print(l_3)
#l_4
l_4=l_4(data2)
print(l_4)

#tau_3
tau_3=l_3/l_2

#tau_4
tau_4=l_4/l_2


### GPD untuk data2 HMSP
LM4=data.frame(l_1=l_1,l_2=l_2,l_3=l_3,
               l_4=l_4,tau_3=tau_3,tau_4=tau_4)
LM4
