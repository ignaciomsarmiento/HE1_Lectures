library(tidyverse)
library(gganimate)
library(ggthemes)


setwd("~/Dropbox/Teaching/2021/HE1/HE1_202001_4/Lectures/Lecture12/Codes")

df <- data.frame(W = as.integer((1:200>100))) %>%
  mutate(X = .5+2*W + rnorm(200)) %>%
  mutate(Y = 0*X + 4*W + 1 + rnorm(200),time="1") %>%
  group_by(W) %>%
  mutate(mean_X=mean(X),mean_Y=mean(Y)) %>%
  ungroup()

#Calculate correlations
before_cor <- paste("1. Start with raw data. Correlation between X and Y: ",round(cor(df$X,df$Y),3),sep='')
after_cor <- paste("6. Analyze what's left! Correlation between X and Y controlling for W: ",round(cor(df$X-df$mean_X,df$Y-df$mean_Y),3),sep='')




#Add step 2 in which X is demeaned, and 3 in which both X and Y are, and 4 which just changes label
dffull <- rbind(
  #Step 1: Raw data only
  df %>% mutate(mean_X=NA,mean_Y=NA,time=before_cor,group=1),
  #Step 2: Add x-lines
  df %>% mutate(mean_Y=NA,time='2. Figure out what differences in X are explained by W',group=2),
  #Step 3: X de-meaned 
  df %>% mutate(X = X - mean_X,mean_X=0,mean_Y=NA,time="3. Remove differences in X explained by W",group=3),
  #Step 4: Remove X lines, add Y
  df %>% mutate(X = X - mean_X,mean_X=NA,time="4. Figure out what differences in Y are explained by W",group=4),
  #Step 5: Y de-meaned
  df %>% mutate(X = X - mean_X,Y = Y - mean_Y,mean_X=NA,mean_Y=0,time="5. Remove differences in Y explained by W",group=5),
  #Step 6: Raw demeaned data only
  df %>% mutate(X = X - mean_X,Y = Y - mean_Y,mean_X=NA,mean_Y=NA,time=after_cor,group=6))


dffull<-dffull %>% mutate(W=factor(W,levels=c(0,1),labels=c("pequeña","grande")))
# ggplot(dffull,aes(y=Y,x=X,color=as.factor(W)))+geom_point()+
#   geom_vline(aes(xintercept=mean_X,color=as.factor(W)))+
#   geom_hline(aes(yintercept=mean_Y,color=as.factor(W)))+
#   guides(color=guide_legend(title="W"))+
#   scale_color_colorblind()+
#   labs(title = 'The Relationship between Y and X, Controlling for a Binary Variable W \n{next_state}')+
#   transition_states(time,transition_length=c(6,16,6,16,6,6),state_length=c(50,22,12,22,12,50),wrap=FALSE)+
#   ease_aes('sine-in-out')+
#   exit_fade()+enter_fade()



before_cor
ggplot(dffull %>% filter(group==1),aes(y=Y,x=X))+
  geom_point(aes(y=Y,x=X,color=as.factor(W)))+
  guides(color=guide_legend(title="Tamaño"))+
  scale_color_manual(values=c("black","blue")) +
  geom_smooth(method="lm", col="red", se=FALSE) +
  xlab("Policias") +
  ylab("Crimen") +
  theme_bw()+
  annotate("text",x=4,y=-1,label="Correlation = 0.716")
ggsave("../figures/frame_1.pdf")  
  



ggplot(dffull %>% filter(group==2),aes(y=Y,x=X))+
  geom_point(aes(y=Y,x=X,color=as.factor(W)))+
  geom_vline(aes(xintercept=mean_X,color=as.factor(W)))+
  geom_hline(aes(yintercept=mean_Y,color=as.factor(W)))+
  guides(color=guide_legend(title="Tamaño"))+
  scale_color_manual(values=c("black","blue")) +
  #geom_smooth(method="lm", col="red", se=FALSE) +
  xlab("Policias") +
  ylab("Crimen") +
  theme_bw()#+
  #annotate("text",x=4,y=-1,label="Correlation = 0.716")
ggsave("../figures/frame_2.pdf")  



ggplot(dffull %>% filter(group==3),aes(y=Y,x=X))+
  geom_point(aes(y=Y,x=X,color=as.factor(W)))+
  geom_vline(aes(xintercept=mean_X,color=as.factor(W)))+
  geom_hline(aes(yintercept=mean_Y,color=as.factor(W)))+
  guides(color=guide_legend(title="Tamaño"))+
  scale_color_manual(values=c("black","blue")) +
  #geom_smooth(method="lm", col="red", se=FALSE) +
  xlab("Policias") +
  ylab("Crimen") +
  theme_bw()#+
#annotate("text",x=4,y=-1,label="Correlation = 0.716")
ggsave("../figures/frame_3.pdf")  




ggplot(dffull %>% filter(group==4),aes(y=Y,x=X))+
  geom_point(aes(y=Y,x=X,color=as.factor(W)))+
  geom_vline(aes(xintercept=mean_X,color=as.factor(W)))+
  geom_hline(aes(yintercept=mean_Y,color=as.factor(W)))+
  guides(color=guide_legend(title="Tamaño"))+
  scale_color_manual(values=c("black","blue")) +
  #geom_smooth(method="lm", col="red", se=FALSE) +
  xlab("Policias") +
  ylab("Crimen") +
  theme_bw()#+
#annotate("text",x=4,y=-1,label="Correlation = 0.716")
ggsave("../figures/frame_4.pdf")  



ggplot(dffull %>% filter(group==5),aes(y=Y,x=X))+
  geom_point(aes(y=Y,x=X,color=as.factor(W)))+
  geom_vline(aes(xintercept=mean_X,color=as.factor(W)))+
  geom_hline(aes(yintercept=mean_Y,color=as.factor(W)))+
  guides(color=guide_legend(title="Tamaño"))+
  scale_color_manual(values=c("black","blue")) +
  #geom_smooth(method="lm", col="red", se=FALSE) +
  xlab("Policias") +
  ylab("Crimen") +
  theme_bw()#+
#annotate("text",x=4,y=-1,label="Correlation = 0.716")
ggsave("../figures/frame_5.pdf")  




ggplot(dffull %>% filter(group==6),aes(y=Y,x=X))+
  geom_point(aes(y=Y,x=X,color=as.factor(W)))+
  guides(color=guide_legend(title="Tamaño"))+
  scale_color_manual(values=c("black","blue")) +
  geom_smooth(method="lm", col="red", se=FALSE) +
  xlab("Policias") +
  ylab("Crimen") +
  theme_bw()+
  annotate("text",x=2,y=-3,label="Correlation = 0.009")
ggsave("../figures/frame_6.pdf")  


round(summary(lm(Y~X, data=dffull %>% filter(group==6)))$coef,4)
round(summary(lm(Y~X, data=dffull %>% filter(group==1)))$coef,4)
