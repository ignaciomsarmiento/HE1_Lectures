##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("tidyverse","ggplot2","ggthemes","stargazer")
lapply(pkg, require, character.only=T)
rm(pkg)




setwd("~/Dropbox/Teaching/2021/HE1/HE1_202001_4/Lectures/Lecture12/Codes")

n<-10000
set.seed(10101)
df <- data.frame(Black = as.integer((1:n>(n/2))))

df <-df %>%
  mutate(Crimen = 2+ .5*Black  + rnorm(n))
         
ggplot(df ,aes(x=Crimen,y=Black))+
  geom_point(aes(x=Crimen,y=Black,color=as.factor(Black)), position=position_jitter(height =.03,width = .05,seed=10101))+
  guides(color=guide_legend(title="Raza"))+
  scale_color_manual(values=c("blue","black")) +
  ylab("Raza") +
  xlab("Crimen") +
  theme_bw() +
  geom_smooth(method="lm", col="red", se=FALSE)


df <-df %>% mutate(Z = 1 - .5*Crimen   + rnorm(n,mean=0,sd=.25),time="1") %>%
  mutate(Y = 1/(1+exp(-Z)),
         Contrato=Y) 


ggplot(df ,aes(x=Crimen,y=Contrato))+
  geom_point(aes(x=Crimen,y=Contrato,color=as.factor(Black)), position=position_jitter(height =.03,width = .05,seed=10101))+
  guides(color=guide_legend(title="Raza"))+
  scale_color_manual(values=c("blue","black")) +
  xlab("Crimen") +
  ylab("Pr(contrato)") +
  theme_bw() +
  geom_smooth(method="lm", col="red", se=FALSE)

reg0<-lm(Contrato~Crimen,df)
reg1<-lm(Contrato~Black,df)
reg2<-lm(Contrato~Black+Crimen,df)
stargazer(reg0,reg1,reg2,type="text")

