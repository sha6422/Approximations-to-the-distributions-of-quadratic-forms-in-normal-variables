library(CompQuadForm)
options(scipen=999)

### Farebrother Method ###
## simulation for Q1 ##
q <- seq(1,11,0.1);F1 <-c()
for (i in 1:length(q))
{
F1[i]=farebrother(q[i] ,c(0.5,0.4,0.1),c(1,2,1),c(1.0,0.6,0.8),eps=10^(-6))$res
}
F11 <- rbind(F1[11], F1[31], F1[51], F1[71],F1[91])

## simulation for Q2 ##
q <- seq(1,11,0.1);F2 <-c()
for (i in 1:length(q))
{
F2[i]=farebrother(q[i] ,c(0.9,0.1),c(1,2),c(0.2,10),eps=10^(-6))$res
}
F22 <- rbind(F2[11], F2[31], F2[51], F2[71],F2[91])

## simulation for Q3 ##
q <- seq(1,21,0.2);F3 <-c()
for (i in 1:length(q))
{
F3[i]=farebrother(q[i] ,c(0.1,0.9),c(1,2),c(0.2,10),eps=10^(-6))$res
}
F33 <- rbind(F3[6], F3[26], F3[46], F3[66],F3[86])

## simulation for Q4 ##
q <- seq(1,11,0.1);F4 <-c()
for (i in 1:length(q))
{
F4[i]=farebrother(q[i] ,c(0.5,0.4,rep(0.01,10)),c(1,2,seq(1,10,1)),c(1.0,0.6,rep(0.8,10)),eps=10^(-6))$res
}
F44 <- rbind(F4[11], F4[31], F4[51], F4[71],F4[91])

## simulation for Q5 ##
q <- seq(1,11,0.1);F5 <-c()
for (i in 1:length(q))
{
F5[i]=farebrother(q[i] ,c(1,(0.6)^4),c(1,1),c(1.0,7.0),eps=10^(-6))$res
}
F55 <- rbind(F5[11], F5[31], F5[51], F5[71],F5[91])

##############################################################
### Imhof Method ###

## simulation for Q1 ##
q <- seq(1,11,0.1);I1 <-c()
for (i in 1:length(q))
{
I1[i]=imhof(q[i] ,c(0.5,0.4,0.1),c(1,2,1),c(1.0,0.6,0.8),epsabs=10^(-6))$Qq
}
I11 <- rbind(I1[11], I1[31], I1[51], I1[71],I1[91])

## simulation for Q2 ##
q <- seq(1,11,0.1);I2 <-c()
for (i in 1:length(q))
{
I2[i]=imhof(q[i] ,c(0.9,0.1),c(1,2),c(0.2,10),epsabs=10^(-6))$Qq
}
I22 <- rbind(I2[11], I2[31], I2[51], I2[71],I2[91])

## simulation for Q3 ##
q <- seq(1,21,0.2);I3 <-c()
for (i in 1:length(q))
{
I3[i]=imhof(q[i] ,c(0.1,0.9),c(1,2),c(0.2,10),epsabs=10^(-6))$Qq
}
I33 <- rbind(I3[6], I3[26], I3[46], I3[66],I3[86])

## simulation for Q4 ##
q <- seq(1,11,0.1);I4 <-c()
for (i in 1:length(q))
{
I4[i]=imhof(q[i] ,c(0.5,0.4,rep(0.01,10)),c(1,2,seq(1,10,1)),c(1.0,0.6,rep(0.8,10)),epsabs=10^(-6))$Qq
}
I44 <- rbind(I4[11], I4[31], I4[51], I4[71],I4[91])

## simulation for Q5 ##
q <- seq(1,11,0.1);I5 <-c()
for (i in 1:length(q))
{
I5[i]=imhof(q[i] ,c(1,(0.6)^4),c(1,1),c(1.0,7.0),epsabs=10^(-6))$Qq
}
I55 <- rbind(I5[11], I5[31], I5[51], I5[71],I5[91])

############################################################
### LTZ Method ###

## simulation for Q1 ##
q <- seq(1,11,0.1);L1 <-c()
for (i in 1:length(q))
{
L1[i]=liu(q[i] ,c(0.5,0.4,0.1),c(1,2,1),c(1.0,0.6,0.8))
}
L11 <- rbind(L1[11], L1[31], L1[51], L1[71],L1[91])

## simulation for Q2 ##
q <- seq(1,11,0.1);L2 <-c()
for (i in 1:length(q))
{
L2[i]=liu(q[i] ,c(0.9,0.1),c(1,2),c(0.2,10))
}
L22 <- rbind(L2[11], L2[31], L2[51], L2[71], L2[91]) 

## simulation for Q3 ##
q <- seq(1,21,0.2);L3 <-c()
for (i in 1:length(q))
{
L3[i]=liu(q[i] ,c(0.1,0.9),c(1,2),c(0.2,10))
}
L33 <- rbind(L3[6], L3[26], L3[46], L3[66],L3[86])

## simulation for Q4 ##
q <- seq(1,11,0.1);L4 <-c()
for (i in 1:length(q))
{
L4[i]=liu(q[i] ,c(0.5,0.4,rep(0.01,10)),c(1,2,seq(1,10,1)),c(1.0,0.6,rep(0.8,10)))
}
L44 <- rbind(L4[11], L4[31], L4[51], L4[71],L4[91])

## simulation for Q5 ##
q <- seq(1,11,0.1);L5 <-c()
for (i in 1:length(q))
{
L5[i]=liu(q[i] ,c(1,(0.6)^4),c(1,1),c(1.0,7.0))
}
L55 <- rbind(L5[11], L5[31], L5[51], L5[71],L5[91])

############################################################
### Pearson three-moment chi-square approximation ###

## simulation for Q1 ##
t <- seq(1,11,0.1); P1<-c()
for(i in 1:length(t))
{
lambda1 <- 0.5
lambda2 <- 0.4
lambda3 <- 0.1
h1 <- 1
h2 <- 2
h3 <- 1
delta1 <- 1
delta2 <- 0.6
delta3 <- 0.8

c1 <- lambda1*h1+lambda2*h2+lambda3*h3+lambda1*delta1+lambda2*delta2+lambda3*delta3
c2 <- lambda1^2*h1+lambda2^2*h2+lambda3^2*h3+2*lambda1^2*delta1+2*lambda2^2*delta2+2*lambda3^2*delta3
c3 <- lambda1^3*h1+lambda2^3*h2+lambda3^3*h3+3*lambda1^3*delta1+3*lambda2^3*delta2+3*lambda3^3*delta3

s1 <- c3/(c2^(3/2))
l_star <- 1/(s1^2)
t_star <- (t[i]-c1)/sqrt(2*c2)
q <- l_star + t_star*sqrt(2*l_star)
df <- l_star

P1[i] <- pchisq(q, df, ncp=0, lower.tail=FALSE)
}
P11 <- rbind(P1[11], P1[31], P1[51], P1[71],P1[91])

## simulation for Q2 ##
t <- seq(1,11,0.1); P2 <-c()
for(i in 1:length(t))
{
lambda1 <- 0.9
lambda2 <- 0.1
h1 <- 1
h2 <- 2
delta1 <- 0.2
delta2 <- 10

c1 <- lambda1*h1+lambda2*h2+lambda1*delta1+lambda2*delta2
c2 <- lambda1^2*h1+lambda2^2*h2+2*lambda1^2*delta1+2*lambda2^2*delta2
c3 <- lambda1^3*h1+lambda2^3*h2+3*lambda1^3*delta1+3*lambda2^3*delta2

s1 <- c3/(c2^(3/2))
l_star <- 1/(s1^2)
t_star <- (t[i]-c1)/sqrt(2*c2)
q <- l_star + t_star*sqrt(2*l_star)
df <- l_star

P2[i] <- pchisq(q, df, ncp=0, lower.tail=FALSE)
}
P22 <- rbind(P2[11], P2[31], P2[51], P2[71], P2[91]) 

## simulation for Q3 ##
t <- seq(1,21,0.2);P3 <-c()
for(i in 1:length(t))
{
lambda1 <- 0.1
lambda2 <- 0.9
h1 <- 1
h2 <- 2
delta1 <- 0.2
delta2 <- 10

c1 <- lambda1*h1+lambda2*h2+lambda1*delta1+lambda2*delta2
c2 <- lambda1^2*h1+lambda2^2*h2+2*lambda1^2*delta1+2*lambda2^2*delta2
c3 <- lambda1^3*h1+lambda2^3*h2+3*lambda1^3*delta1+3*lambda2^3*delta2

s1 <- c3/(c2^(3/2))
l_star <- 1/(s1^2)
t_star <- (t[i]-c1)/sqrt(2*c2)
q <- l_star + t_star*sqrt(2*l_star)
df <- l_star

P3[i] <- pchisq(q, df, ncp=0, lower.tail=FALSE)
}
P33 <- rbind(P3[6], P3[26], P3[46], P3[66],P3[86]) 

## simulation for Q4 ##
t <- seq(1,11,0.1); P4 <- c()
for( i in 1:length(t))
{
lambda1 <- 0.5
lambda2 <- 0.4
lambda3 <- lambda4 <- lambda5 <- lambda6 <- lambda7 <- lambda8 <-lambda9 <- lambda10 <- lambda11 <- lambda12 <- 0.01
h1 <- 1
h2 <- 2
h3 <- 1; h4 <- 2; h5 <- 3; h6 <- 4; h7 <- 5; h8 <- 6; h9 <- 7; h10 <- 8; h11 <- 9; h12 <- 10
delta1 <- 1
delta2 <- 0.6
delta3 <- delta4 <- delta5 <- delta6 <- delta7 <- delta8 <- delta9 <- delta10 <- delta11 <- delta12 <- 0.8 

c1 <- lambda1*h1+lambda2*h2+lambda3*h3+lambda4*h4+lambda5*h5+lambda6*h6+lambda7*h7+lambda8*h8+lambda9*h9+lambda10*h10
+lambda11*h11+lambda12*h12+lambda1*delta1+lambda2*delta2+lambda3*delta3+lambda4*delta4+lambda5*delta5+lambda6*delta6
+lambda7*delta7+lambda8*delta8+lambda9*delta9+lambda10*delta10+lambda11*delta11+lambda12*delta12
c2 <- lambda1^2*h1+lambda2^2*h2+lambda3^2*h3+lambda4^2*h4+lambda5^2*h5+lambda6^2*h6+lambda7^2*h7+lambda8^2*h8
+lambda9^2*h9+lambda10^2*h10+lambda11^2*h11+lambda12^2*h12+2*lambda1^2*delta1+2*lambda2^2*delta2+2*lambda3^2*delta3
+2*lambda4^2*delta4+2*lambda5^2*delta5+2*lambda6^2*delta6+2*lambda7^2*delta7+2*lambda8^2*delta8+2*lambda9^2*delta9
+2*lambda10^2*delta10+2*lambda11^2*delta11+2*lambda12^2*delta12
c3 <- lambda1^3*h1+lambda2^3*h2+lambda3^3*h3+lambda4^3*h4+lambda5^3*h5+lambda6^3*h6+lambda7^3*h7+lambda8^3*h8
+lambda9^3*h9+lambda10^3*h10+lambda11^3*h11+lambda12^3*h12+3*lambda1^3*delta1+3*lambda2^3*delta2+3*lambda3^3*delta3
+3*lambda4^3*delta4+3*lambda5^3*delta5+3*lambda6^3*delta6+3*lambda7^3*delta7+3*lambda8^3*delta8+3*lambda9^3*delta9
+3*lambda10^3*delta10+3*lambda11^3*delta11+3*lambda12^3*delta12

s1 <- c3/(c2^(3/2))
l_star <- 1/(s1^2)
t_star <- (t[i]-c1)/sqrt(2*c2)
q <- l_star + t_star*sqrt(2*l_star)
df <- l_star

P4[i] <- pchisq(q, df, ncp=0, lower.tail=FALSE)
}
P44 <- rbind(P4[11], P4[31], P4[51], P4[71],P4[91])

## simulation for Q5 ##
t <- seq(1,11,0.1); P5 <-c()
for(i in 1:length(t))
{
lambda1 <- 1.0
lambda2 <- 0.6^4
h1 <- 1
h2 <- 1
delta1 <- 1.0
delta2 <- 7.0

c1 <- lambda1*h1+lambda2*h2+lambda1*delta1+lambda2*delta2
c2 <- lambda1^2*h1+lambda2^2*h2+2*lambda1^2*delta1+2*lambda2^2*delta2
c3 <- lambda1^3*h1+lambda2^3*h2+3*lambda1^3*delta1+3*lambda2^3*delta2

s1 <- c3/(c2^(3/2))
l_star <- 1/(s1^2)
t_star <- (t[i]-c1)/sqrt(2*c2)
q <- l_star + t_star*sqrt(2*l_star)
df <- l_star

P5[i] <- pchisq(q, df, ncp=0, lower.tail=FALSE)
}
P55 <- rbind(P5[11], P5[31], P5[51], P5[71], P5[91]) 

#######################################################################
### Saddle point method ###
library(rootSolve)

## simulation for Q1 ##
t <- seq(1,11,0.1); S1 <- c()
for ( i in 1:length(t))
{
lambda1 <- 0.5
lambda2 <- 0.4
lambda3 <- 0.1
h1<- 1
h2 <- 2
h3 <- 1
delta1 <- 1
delta2 <- 0.6
delta3 <- 0.8

K <-function(x)
(-(1/2)*h1*log(1-2*x*lambda1)-(1/2)*h2*log(1-2*x*lambda2)-(1/2)*h3*log(1-2*x*lambda3)
+ delta1*lambda1*x/(1-2*x*lambda1)+delta2*lambda2*x/(1-2*x*lambda2)+delta3*lambda3*x/(1-2*x*lambda3))
g <- function(x) {}       ## first derivative ##
body(g) <- D(body(K), 'x')
h <- function(x) {}       ## second derivative ##
body (h) <- D(body(g), 'x')

g.new <- function(x) (g(x)-t[i])
epsi <- uniroot.all(g.new, c(-10,10))[uniroot.all(g.new, c(-10,10))<min(1/(2*lambda1),1/(2*lambda2),
1/(2*lambda3))]
w <- sign(epsi)*(2*(epsi*t[i]-K(epsi)))^(1/2)
v <- epsi*(h(epsi))^(1/2)
S1[i] <-pnorm((w+(1/w)*log(v/w)),mean=0, sd=1, lower.tail=FALSE)
}
S11 <- rbind(S1[11], S1[31], S1[51], S1[71], S1[91])

## simulation for Q2 ##
t <- seq(1,11,0.1); S2 <- c()
for ( i in 1:length(t))
{
lambda1 <- 0.9
lambda2 <- 0.1
h1<- 1
h2 <- 2
delta1 <- 0.2
delta2 <- 10

K <-function(x)
(-(1/2)*h1*log(1-2*x*lambda1)-(1/2)*h2*log(1-2*x*lambda2)+ delta1*lambda1*x/(1-2*x*lambda1)+
delta2*lambda2*x/(1-2*x*lambda2))
g <- function(x) {}       ## first derivative ##
body(g) <- D(body(K), 'x')
h <- function(x) {}       ## second derivative ##
body (h) <- D(body(g), 'x')

g.new <- function(x) (g(x)-t[i])
epsi <- uniroot.all(g.new, c(-10,10))[uniroot.all(g.new, c(-10,10))<min(1/(2*lambda1),1/(2*lambda2))]
w <- sign(epsi)*(2*(epsi*t[i]-K(epsi)))^(1/2)
v <- epsi*(h(epsi))^(1/2)
S2[i] <-pnorm((w+(1/w)*log(v/w)),mean=0, sd=1, lower.tail=FALSE)
}
S22 <- rbind(S2[11], S2[31], S2[51], S2[71], S2[91])


## simulation for Q3 ##
t <- seq(1,21,0.2); S3 <- c()
for ( i in 1:length(t))
{
lambda1 <- 0.1
lambda2 <- 0.9
h1<- 1
h2 <- 2
delta1 <- 0.2
delta2 <- 10

K <-function(x)
(-(1/2)*h1*log(1-2*x*lambda1)-(1/2)*h2*log(1-2*x*lambda2)+ delta1*lambda1*x/(1-2*x*lambda1)+
delta2*lambda2*x/(1-2*x*lambda2))
g <- function(x) {}       ## first derivative ##
body(g) <- D(body(K), 'x')
h <- function(x) {}       ## second derivative ##
body (h) <- D(body(g), 'x')

g.new <- function(x) (g(x)-t[i])
epsi <- uniroot.all(g.new, c(-10,10))[uniroot.all(g.new, c(-10,10))<min(1/(2*lambda1),1/(2*lambda2))]
w <- sign(epsi)*(2*(epsi*t[i]-K(epsi)))^(1/2)
v <- epsi*(h(epsi))^(1/2)
S3[i] <-pnorm((w+(1/w)*log(v/w)),mean=0, sd=1, lower.tail=FALSE)
}
S33 <- rbind(S3[6], S3[26], S3[46], S3[66], S3[86])

## simulation for Q4 ##
t <- seq(1,11,0.1); S4 <- c()
for ( i in 1:length(t))
{
lambda1 <- 0.5
lambda2 <- 0.4
lambda3 <- lambda4 <- lambda5 <- lambda6 <- lambda7 <- lambda8 <-lambda9 <- lambda10 <- lambda11 <- lambda12 <- 0.01
h1 <- 1
h2 <- 2
h3 <- 1; h4 <- 2; h5 <- 3; h6 <- 4; h7 <- 5; h8 <- 6; h9 <- 7; h10 <- 8; h11 <- 9; h12 <- 10
delta1 <- 1
delta2 <- 0.6
delta3 <- delta4 <- delta5 <- delta6 <- delta7 <- delta8 <- delta9 <- delta10 <- delta11 <- delta12 <- 0.8 

K <-function(x)
(-(1/2)*h1*log(1-2*x*lambda1)-(1/2)*h2*log(1-2*x*lambda2)-(1/2)*h3*log(1-2*x*lambda3)-(1/2)*h4*log(1-2*x*lambda4)
-(1/2)*h5*log(1-2*x*lambda5)-(1/2)*h6*log(1-2*x*lambda6)-(1/2)*h7*log(1-2*x*lambda7)-(1/2)*h8*log(1-2*x*lambda8)
-(1/2)*h9*log(1-2*x*lambda9)-(1/2)*h10*log(1-2*x*lambda10)-(1/2)*h11*log(1-2*x*lambda11)-(1/2)*h12*log(1-2*x*lambda12)
+ delta1*lambda1*x/(1-2*x*lambda1)+delta2*lambda2*x/(1-2*x*lambda2)+delta3*lambda3*x/(1-2*x*lambda3)
+delta4*lambda4*x/(1-2*x*lambda4)+delta5*lambda5*x/(1-2*x*lambda5)+delta6*lambda6*x/(1-2*x*lambda6)+delta7*lambda7*x/(1-2*x*lambda7)
+delta8*lambda8*x/(1-2*x*lambda8)+delta9*lambda9*x/(1-2*x*lambda9)+delta10*lambda10*x/(1-2*x*lambda10)
+delta11*lambda11*x/(1-2*x*lambda11)+delta12*lambda12*x/(1-2*x*lambda12))
g <- function(x) {}       ## first derivative ##
body(g) <- D(body(K), 'x')
h <- function(x) {}       ## second derivative ##
body (h) <- D(body(g), 'x')

g.new <- function(x) (g(x)-t[i])
epsi <- uniroot.all(g.new, c(-10,10))[uniroot.all(g.new, c(-10,10))<min(1/(2*lambda1),1/(2*lambda2),1/(2*lambda3))]
w <- sign(epsi)*(2*(epsi*t[i]-K(epsi)))^(1/2)
v <- epsi*(h(epsi))^(1/2)
S4[i] <-pnorm((w+(1/w)*log(v/w)),mean=0, sd=1, lower.tail=FALSE)
}
S44 <- rbind(S4[11], S4[31], S4[51], S4[71], S4[91])

## simulation for Q5 ##
t <- seq(1,21,0.2); S5 <- c()
for ( i in 1:length(t))
{
lambda1 <- 1.0
lambda2 <- 0.6^4
h1<- 1
h2 <- 1
delta1 <- 1.0
delta2 <- 7.0

K <-function(x)
(-(1/2)*h1*log(1-2*x*lambda1)-(1/2)*h2*log(1-2*x*lambda2)+ delta1*lambda1*x/(1-2*x*lambda1)+
delta2*lambda2*x/(1-2*x*lambda2))
g <- function(x) {}       ## first derivative ##
body(g) <- D(body(K), 'x')
h <- function(x) {}       ## second derivative ##
body (h) <- D(body(g), 'x')

g.new <- function(x) (g(x)-t[i])
epsi <- uniroot.all(g.new, c(-10,10))[uniroot.all(g.new, c(-10,10))<min(1/(2*lambda1),1/(2*lambda2))]
w <- sign(epsi)*(2*(epsi*t[i]-K(epsi)))^(1/2)
v <- epsi*(h(epsi))^(1/2)
S5[i] <-pnorm((w+(1/w)*log(v/w)),mean=0, sd=1, lower.tail=FALSE)
}
S55 <- rbind(S5[11], S5[31], S5[51], S5[71], S5[71])

q <- c(2,4,6,8,10,2,4,6,8,10,2,6,10,14,18,2,4,6,8,10,2,4,6,8,10)
Q <- c(rep("Q1",5),rep("Q2",5),rep("Q3",5),rep("Q4",5),rep("Q5",5))
F <- rbind( F11,F22,F33,F44,F55)
I <- rbind( I11,I22,I33,I44,I55)
P <- rbind( P11,P22,P33,P44,P55)
L <- rbind( L11,L22,L33,L44,L55)
S <- rbind( S11,S22,S33,S44,S55)

AE_I <- round(abs(F-I),7)
AE_P <- round(abs(F-P),7)
AE_L <- round(abs(F-L),7)
AE_S <- round(abs(F-S),7)

RE_I <- round(AE_I/F,7)*100
RE_P <- round(AE_P/F,7)*100
RE_L <- round(AE_L/F,7)*100
RE_S <- round(AE_S/F,7)*100
table_ae <- cbind.data.frame( "Quadratic form"=Q,"q"=q, "Farebrother"=F,"Imhof"=I,"AE_I"=AE_I,"Pearson"=P,"AE_P"=AE_P, 
"LTZ"=L, "AE_L"=AE_L, "Saddle"=S,"AE_S"=AE_S)
table_ae

table_re <- cbind.data.frame( "Quadratic form"=Q,"q"=q, "Farebrother"=F,"Imhof"=I,"RE_I(%)"=RE_I,"Pearson"=P,"RE_P(%)"=RE_P, 
"LTZ"=L, "RE_L(%)"=RE_L, "Saddle"=S,"RE_S(%)"=RE_S)
table_re

F_Q1 <- F[1:5];I_Q1 <- I[1:5];P_Q1 <- P[1:5];L_Q1 <- L[1:5];S_Q1 <- S[1:5]
F_Q2 <- F[6:10];I_Q2 <- I[6:10];P_Q2 <- P[6:10];L_Q2 <- L[6:10];S_Q2 <- S[6:10]
F_Q3 <- F[11:15];I_Q3 <- I[11:15];P_Q3 <- P[11:15];L_Q3 <- L[11:15];S_Q3 <- S[11:15]
F_Q4 <- F[16:20];I_Q4 <- I[16:20];P_Q4 <- P[16:20];L_Q4 <- L[16:20];S_Q4 <- S[16:20]
F_Q5 <- F[21:25];I_Q5 <- I[21:25];P_Q5 <- P[21:25];L_Q5 <- L[21:25];S_Q5 <- S[21:25]

## compare Q1 and Q4 ##
attach(mtcars)
par(mfrow=c(1,3))

qqplot(P1,F1,plot.it=TRUE,xlab="Pearson's approximation",ylab="Farebrother's approximation",col='red')
par(new=TRUE)
qqplot(P4,F4, plot.it=TRUE,axes=FALSE,xlab="",ylab="",col='blue')

qqplot(L1,F1,plot.it=TRUE,xlab="LTZ's approximation",ylab="Farebrother's approximation",col='red')
par(new=TRUE)
qqplot(L4,F4, plot.it=TRUE,axes=FALSE,xlab="",ylab="",col='blue')

qqplot(S1,F1, plot.it=TRUE,xlab="Saddle point approximation",ylab="Farebrother's approximation",col='red')
par(new=TRUE)
qqplot(S4,F4, plot.it=TRUE,axes=FALSE,xlab="",ylab="",col='blue')



## compare Q2 and Q3 ##
attach(mtcars)
par(mfrow=c(1,3))

qqplot(P2,F2,plot.it=TRUE,xlab="Pearson's approximation",ylab="Farebrother's approximation",col='red')
par(new=TRUE)
qqplot(P3,F3, plot.it=TRUE,axes=FALSE,xlab="",ylab="",col='blue')

qqplot(L2,F2,plot.it=TRUE,xlab="LTZ's approximation",ylab="Farebrother's approximation",col='red')
par(new=TRUE)
qqplot(L3,F3, plot.it=TRUE,axes=FALSE,xlab="",ylab="",col='blue')

qqplot(S2,F2, plot.it=TRUE,xlab="Saddle point approximation",ylab="Farebrother's approximation",col='red')
par(new=TRUE)
qqplot(S3,F3, plot.it=TRUE,axes=FALSE,xlab="",ylab="",col='blue')

##Q5##
qqplot(P5,F5,plot.it=TRUE,xlab=" Pearson & LTZ& Saddle point approximation",ylab="Farebrother's approximation",col='red')
par(new=TRUE)
qqplot(L5,F5, plot.it=TRUE,axes=FALSE,xlab="",ylab="",col='blue')
par(new=TRUE)
qqplot(S5,F5, plot.it=TRUE,axes=FALSE,xlab="",ylab="",col='green')








