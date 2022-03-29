studentnumber =0819511
fulldata = read.csv("HousePrices.txt", sep = " ", header = TRUE)
digitsum = function(x) sum(floor(x/10^(0:(nchar(x)-1)))%%10)

set.seed(studentnumber)
mysum = digitsum(studentnumber)

if((mysum %% 2) == 0) { # number is even
  rownumbers = sample(1:327,150,replace=F)
} else { # number is odd
  rownumbers = sample(309:585,150,replace=F)
}
mydata = fulldata[rownumbers,]