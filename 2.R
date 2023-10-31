##########
# SAMPLE #
##########
x <- 1:10
x
sample(x)
# Simuliramo bacanje novcica
coin <- c("Heads", "Tails")
sim <- sample(coin, size = 100, replace = T, prob=c(1/3,2/3))
sum(sim == "Heads") # koliko puta je pala glava
#######
# 8.
s <- sample(1:6, size = 1000, replace = T)
sum(s==6)/1000
sum(s<5)/1000
#######
# 9.
s1 <- sample(1:6, size =  1000, replace = T, prob = 1:6/21)
sum(s1%%2==0)/length(s1)
#######
# 10.
igra <- function(p){
  sim <- sample(c("G", "P"), replace = T, prob=c(p,1-p),size = 2)
  if (sim[1]=="G" & sim[2]=="P") return(1)
  else if (sim[1]=="P" & sim[2]=="G") return(2)
  else
    return(0)
}
sum(replicate(1000,igra(p=0.9))==1)
sum(replicate(1000,igra(p=0.9))==2)
