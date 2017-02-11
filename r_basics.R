a <- 5
b <- 3
a + b
c = Inf

occur <- matrix(c(1,1,1),ncol=3,byrow=TRUE)
occur

d = random(3,4)
e = ones(3,4)
ones(3,4) *9

x <- vector(mode="numeric", length=3)
x
seq1 <- seq(1:6)
mat1 <- matrix(seq1, nrow=2)
mat1

s <- matrix(1:6, ncol=3, byrow=TRUE)
st <- rbind(c(1,1,1),s)
st

stu <- rbind(st,(7:9))
stu

stu[2:3,1:2]

stu[3:4,2:3]

stu[(nrow(stu)-1):nrow(stu),(ncol(stu)-1):ncol(stu)]
stu[3,2]

stu[3,] < NA

a <- matrix(c(1,1,1,2,3,4), nrow=2, byrow=T)
a

t <- matrix(c(1,5),ncol=1)
t

dim(a)
dim(t(t))

a *  (t)

# 3 columns 20 rows from normal dist
replicate(3, rnorm(20)) 

set.seed(133)

b <- (1:10)

for (year in c(2010,2011,2012,2013,2014,2015)){
  print(paste("The year is", year))
}

years <- c(2010,2011,2012,2013,2014,2015)
for (year in 1:length(years)){
  print(paste("The year is", years[year]))
}

D <- matrix(c(2,-2,1,2,3,1),2,3)
A <- matrix(c(2,3,-2,1,2,2),3,2)
A %*% D

theta <- matrix(c(0,4), ncol=1)
theta
X <- matrix(c(1,1,1,1,2,3), ncol=3, nrow=2, byrow=T)
X
t(theta) %*% X
