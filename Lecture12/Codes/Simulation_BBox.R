##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("tidyverse","ggplot2","gganimate","ggthemes")
lapply(pkg, require, character.only=T)
rm(pkg)




setwd("~/Dropbox/Teaching/2021/HE1/HE1_202001_4/Lectures/Lecture12/Codes")

n<-500
set.seed(10101)
df <- data.frame(Black = as.integer((1:n>(n/2)))) %>%
  mutate(Crimen = 2+ 10*Black + runif(n,0,10),
         X=Crimen,
         W=Black) %>%
  #mutate(Y = 0*X + 4*W + 1 + rnorm(200),time="1") %>%
  mutate(Z = 3+0*Black -0.2*Crimen   + rnorm(n),time="1") %>%
  mutate(Y = 1/(1+exp(-Z)),
         Contrato=Y) %>% 
  group_by(Black) %>%
  mutate(mean_X=mean(Crimen),mean_Y=mean(Y), mean_Black=mean(Black),mean_Contrato=mean(Contrato)) %>%
  ungroup()




#Calculate correlations
#before_cor <- paste("1. Start with raw data. Correlation between X and Y: ",round(cor(df$X,df$Y),3),sep='')




#Add step 2 in which X is demeaned, and 3 in which both X and Y are, and 4 which just changes label
dffull <- rbind(
  #Step 1: Raw data only
  df %>% mutate(mean_X=NA,mean_Y=mean_Y,time="1.Start with raw data. ",group=1),
  #Step 2: Add x-lines
  df %>% mutate(mean_Y=mean_Y,time='2. Figure out what differences in X are explained by W',group=2),
  #Step 3: X de-meaned 
  df %>% mutate(Black = Black ,mean_Black=0,
                X = X - mean_X,mean_X=0,
                mean_Y=mean_Y,time="3. Remove differences in X explained by W",group=3),
  df %>% mutate(Black = Black ,mean_Black=0,
                X = X - mean_X,mean_X=0,
                Y = Y - mean_Y,mean_X=0,time="3. Remove differences in X explained by W",group=4))


summary(df$Y)
reg_before<-lm(Y~Black,data=dffull %>% filter(group==1))
summary(reg_before)
before_cor <- paste("Diferencia: ",round(reg_before$coefficients["Black"],2 ),sep='')
before_cor
reg_after<-lm(Y~Black+Crimen,data=dffull %>% filter(group==1))
summary(reg_after)
after_cor <- paste("Diferencia: ",round(reg_after$coefficients["Black"],2 ),sep='')
after_cor


dffull %>% group_by(group,Black) %>% summarize(Y=mean(Y)) %>% filter(group==1)
0.79-0.40
ggplot(dffull %>% filter(group==1),aes(y=Y,x=Black))+
  geom_point(aes(y=Y,x=Black,color=as.factor(Black)), position=position_jitter(height =.01,width = .03,seed=10101))+
  guides(color=guide_legend(title="Raza"))+
  scale_color_manual(values=c("blue","black")) +
  xlab("Raza") +
  ylab("Pr(contrato)") +
  theme_bw() +
  geom_smooth(method="lm", col="red", se=FALSE)  +
  annotate("text",x=0.5,y=0,label=before_cor)
ggsave("../figures/frame_1.pdf")  
  

ggplot(dffull %>% filter(group==1),aes(y=Y,x=Black))+
  geom_point(aes(y=Y,x=Black,color=as.factor(Black)), position=position_jitter(height =.01,width = .03,seed=10101))+
  geom_hline(aes(yintercept=mean_Y,color=as.factor(Black)))+
  guides(color=guide_legend(title="Raza"))+
  scale_color_manual(values=c("blue","black")) +
  xlab("Raza") +
  ylab("Pr(contrato)") +
  theme_bw() +
  #geom_smooth(method="lm", col="red", se=FALSE)  +
  annotate("text",x=0.5,y=0,label=before_cor)
ggsave("../figures/frame_2.pdf")  

ggplot(dffull %>% filter(group==1),aes(y=Y,x=X))+
  geom_point(aes(y=Y,x=X,color=as.factor(W)))+
  #geom_vline(aes(xintercept=mean_X,color=as.factor(W)))+
  #geom_hline(aes(yintercept=mean_Y,color=as.factor(W)))+
  guides(color=guide_legend(title="Raza"))+
  scale_color_manual(values=c("blue","black")) +
  xlab("Crimen") +
  ylab("Pr(contrato)") +
  theme_bw()#+
ggsave("../figures/frame_3.pdf")  



ggplot(dffull %>% filter(group==2),aes(y=Y,x=X,group=as.factor(W),color=as.factor(W)))+
  geom_point(aes(y=Y,x=X,color=as.factor(W)))+
  geom_vline(aes(xintercept=mean_X,color=as.factor(W)))+
  #geom_hline(aes(yintercept=mean_Y,color=as.factor(W)))+
  guides(color=guide_legend(title="Raza"))+
  scale_color_manual(values=c("blue","black")) +
  #geom_smooth(method="lm", col="red", se=FALSE) +
  xlab("Crimen") +
  ylab("Pr(contrato)") +
  theme_bw()#+
ggsave("../figures/frame_4.pdf")  




ggplot(dffull %>% filter(group==3),aes(y=Y,x=X,color=as.factor(W),group=as.factor(W)))+
  geom_point(aes(y=Y,x=X,color=as.factor(W)))+
  #geom_vline(aes(xintercept=mean_X,color=as.factor(W)))+
  #geom_hline(aes(yintercept=mean_Y,color=as.factor(W)))+
  guides(color=guide_legend(title="Raza"))+
  scale_color_manual(values=c("blue","black")) +
  geom_smooth(method="lm", col="red", se=FALSE) +
  xlab("Crimen") +
  ylab("Pr(contrato)") +
  theme_bw()#+
ggsave("../figures/frame_5.pdf")  



ggplot(dffull %>% filter(group==4),aes(y=Y,x=X,color=as.factor(W),group=as.factor(W)))+
  geom_point(aes(y=Y,x=X,color=as.factor(W)))+
  #geom_vline(aes(xintercept=mean_X,color=as.factor(W)))+
  #geom_hline(aes(yintercept=mean_Y,color=as.factor(W)))+
  guides(color=guide_legend(title="Raza"))+
  scale_color_manual(values=c("blue","black")) +
  geom_smooth(method="lm", col="red", se=FALSE) +
  xlab("Crimen") +
  ylab("Pr(contrato)") +
  theme_bw()#+
ggsave("../figures/frame_6.pdf")  

