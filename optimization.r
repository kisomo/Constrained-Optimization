
#objective function

g <- function(x1,x2)
{
  x1^2 + x2^2 -3*x1 +x2 +2
}

gb <- function(x)g(x[1],x[2])

x1 <- seq(-2,2,len=51)
x2 <- seq(-2,2,len=51)
gz <- outer(x1,x2,g)
contour(x1,x2,gz)
polygon(c(0,0,1),c(1,2,1),col="grey",density = c(30,40))

#constraints

# x1 >=0
# x2 >=0
# x1 + x2 =< 2

A = matrix(c(1,0,0,1,-1,-1),3,2,byrow = T)
b = c(0,1,-2)

constrOptim(c(0.1,1.1),gb,NULL,A,b)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

f <- function(x,y,z)
{
  x^2 + y + z^2 + 2
}

fb <- function(x)f(x[1],x[2],x[3])

#x <- seq(-2,2,len=51)
#y <- seq(-2,2,len=51)
#z <- seq(-2,2,len=51)

#fz <- outer(x,y,z,f)
#contour(x,y,z,fz)
#polygon(c(0,0,1),c(1,2,1),col="grey",density = c(30,40))

# x >= 0
# 2 >= y >= 0
# z >= 1
# x + y + z <= 4

A = matrix(c(1,0,0,0,-1,0,0,1,0,0,0,1,-1,-1,-1),5,3,byrow = T)
b = c(0,-2,0,1,-4)

constrOptim(c(1,1,1.5),fb,NULL,A,b)
#constrained optimization output here

optim(c(1,1,1),fb)



