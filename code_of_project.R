# ANAL DED

#askhsh-1

d <- c(73,6,77,81,91,101,135,61,65,68,18,20,23,12,14,18,23,26,26,27,2,3,3,40,41,41,6,10,11,12,37,38,38,6,73,6,51)


#i
library(moments)
mean(d);median(d);mode(d);r<-max(d)-min(d);
skewness(d);IQR(d);max(table(d));table(d);
var(d)

#ii
par(mfrow=c(2,2))
hist(d)
boxplot(d)
qqnorm(d)
qqline(d)

#iii
ks.test(unique(d),'pnorm',mean(d),sd(d))
library(nortest)
lillie.test(d)
shapiro.test(d)

#iv
a = (mean(d)^2)/(mean(d^2)-(mean(d))^2)
b = (mean(d^2)-mean(d)^2)/mean(d)

f <- function(x,a,b){
Ga <- prod(1:(a-1))
fun <- (1/((b^a)*Ga))*(x^(a-1))*exp(-(x/b))
return(fun)
}



br <- seq(0,140,20)
hist(d,breaks=br)
lines(seq(0,150),f(seq(0,150,1),a,b),col='red')

#v
plot(sort(d),qexp((0:(length(d)-1))/length(d),0.027),pch=19)
lines(c(0,140),c(0,140),col='red')

ks.test(d,'pexp',0.027)

#vi
bstat = length(d[d<20])
qbinom(0.025,77,0.5)
qbinom(0.025,77,0.5)



################################################



## 2


set.seed(20022)
deigma <- matrix(rep(0,1110*5),c(5,1110))
mesos <- c() # ΜΕΣΟΣ ΔΕΙΓΜΑΤΟΣ

for (i in 1:dim(deigma)[2]){
deigma[,i] <- rnorm(5,10,3)
mesos[i] <- mean(deigma[,i])
}

anwgrammh <- 10+(3*3/sqrt(5))
katwgrammh <- 10-(3*3/sqrt(5))

plot(mesos,type='l',col='blue') # diagramma
abline(anwgrammh,0,col='red')
abline(katwgrammh,0,col='red')


thesi <- which((mesos<katwgrammh)|(mesos>anwgrammh)) 

outliers <- mesos[thesi]

cat('false alarms =',length(outliers),'\n')



######################################################




### ANAL-DED

#Askhsh-3


data <- read.table('/home/user/Downloads/ANAL-DED/Ergasia/babies.txt',header=T)



j<-0
count<-c()
sthlh <- c(5,10,12,13,15,17,18)
del <-c(999,99,99,999,99,99,999)
for (i in 1:dim(data)[1]){
if (sum(data[i,sthlh]==del)){
j <- j +1
count[j] <- i
}
}
d_new <- data[-c(count),]

#i
attach(d_new)
model <- lm(wt ~ gestation+ age+ ht+ wt1+ dage+ dht+ dwt )
summary(model)

#ii
# r_sqr ~= 0.2117

#iii
# p_value(F_test) <2.2*10^(-16) < a=0.01

#iv
expected(wt) = -101.907+ 270*0.45031+ 29*0.134+ 63*1.223+ 220*0.030+ 36*0.06- 74*0.078+ 230*0.078
\begin{center}
\begin{multiline}
$\hat{wt}$ = -101.907+270\cdot 0.45031+29\cdot 0.134+63\cdot 1.223+220\cdot 0.030+36\cdot 0.06- 74\cdot 0.078 + 230\cdot 0.078 = 121.539
\end{multiline}
\end{center}
\\
$$\text{\greektext Δ.Ε.} = \hat{wt} \pm t_{\frac{a}{2},684} \cdot \hat{\sigma}_{resid} \Rightarrow $$\\
$$\text{\greektext Δ.Ε.} = [117.3 , 125.777]$$\\



df <- data.frame(gestation=270 , age=29, ht=63, wt1=220, dage=36, dht=74, dwt=230 )
predict(model,newdata=df,interval='confidence',level=0.99)


#v
model1_prin <- lm(wt ~ gestation+ age+ ht)
model1_meta <- lm(wt ~ gestation+ age+ ht+wt1+dage+dht+dwt)
anova(model1_prin,model1_meta)

#vi
step(model)


modelaic <- lm(wt~gestation+age+ht+dwt)
#aic = 3927.41

#vii
summary(model)

0.45031 +- [qt(0.95,698)=1.647055]*[se(b1)=0.039]


conf.int(b1)= [0.386,0.514]

#viii
vcov(model)

#ix
[381,498]

infl.point[151]
infl_point <- d_new[151,]
plot(cooks.distance(model))
print(which(cooks.distance(model))>0.15)

#x
res <- resid(model)
hist(res)
lines(dnorm(seq(-70,70),0,1)))
qqnorm(res)
qqline(res)
shapiro.test(res)
lillie.test(res)


