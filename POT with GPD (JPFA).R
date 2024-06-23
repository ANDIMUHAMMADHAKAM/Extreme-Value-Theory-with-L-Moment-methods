library(readxl)
POT=read_excel("POT.xlsx")

# untuk l_1
l_1 <- function(df) {
  n <- length(df)
  df <- sort(df)
  hasil <- 0
  for (i in 1:n) {
    hasil <- hasil + df[i]/n
  }
  return(hasil)
}

# Untuk l_2
l_2 <- function(df) {
  n <- length(df)
  df <- sort(df)  #Mengurutukan df
  hasil <- 0  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      hasil <- hasil + (df[j] - df[i])
    }
  }
  hasil <- hasil / (2 * choose(n, 2)) 
  return(hasil)
}

# Untuk l_3
l_3 <- function(df) {
  n <- length(df)
  df <- sort(df)  # Mengurutkan df
  hasil <- 0  
  for (i in 1:(n-2)) {
    for (j in (i+1):(n-1)) {
      for (k in (j+1):n) {
        hasil <- hasil + (df[k] - 2 * df[j] + df[i])
      }
    }
  }
  hasil <- hasil / (3 * choose(n, 3)) 
  return(hasil)
}

#untuk l_4
# Fungsi untuk menghitung persamaan l_4
l_4 <- function(df) {
  n <- length(df)
  df <- sort(df)  # Mengurutkan df
  hasil <- 0  
  for (i in 1:(n-3)) {
    for (j in (i+1):(n-2)) {
      for (k in (j+1):(n-1)) {
        for (l in (k+1):n) {
          hasil <- hasil + (df[l] - 3 * df[k] + 3 * df[j] - df[i])
        }
      }
    }
  }
  hasil <- hasil / (4 * choose(n, 4)) 
  return(hasil)
}

##2. Untuk df JPFA
df=POT$JPFA
#Untuk l_1
l_1=l_1(df)
#Untuk l_2
l_2=l_2(df)
#Untuk l_3
l_3=l_3(df)
#Untuk l_4
l_4=l_4(df)

#tau_3
tau_3=l_3/l_2
#tau_4
tau_4=l_4/l_2

##GEV untuk df JPFA
LM3=data.frame(l_1=l_1,l_2=l_2,l_3=l_3,
               l_4=l_4,tau_3=tau_3,tau_4=tau_4)
LM3
