library(tidyverse)
library(readxl)
library(scales)
## read data, painting first
expl <- read.csv("data/new_exp/explP.csv",header = FALSE)
fam <- read_excel("data/familiarity/familiarity.xlsx", sheet = "painting_used") 
## for quote
expl <- read.csv("data/new_exp/explQ.csv",header = FALSE)
fam <- read_excel("data/familiarity/familiarity.xlsx", sheet = "quote_used")

### add colname
colnames(expl) <- c("picID",'expl_phase','rt','payoff','schemaID')
unique(expl$picID) # 36 *15 pictutres, 15 *28 quotes 
## we have 36 pic for each schementa, 1-36 for schema 1, 1-28 for quote 1

#### just check the data structure
expl %>% group_by(schemaID) %>% summarise(picnum = n_distinct(picID))
picinorder <- expl %>% group_by(schemaID) %>% 
  distinct(picID) %>% arrange(schemaID,picID)
####

### assign participant id
rndb2 <- expl %>% mutate(new = lag(expl_phase)==expl_phase)
rndb2<-rndb2 %>% mutate(rk = row_number()) # add row number
ind<-rndb2 %>% filter(new==FALSE & expl_phase==1) %>% select(rk)
ind <- ind$rk
ind <- c(ind,length(rndb2$expl_phase)+1) # last phase should be added
participantid <- rep(1,ind[1]-1)
for (i in 2:length(ind)){
  participantid <- c(participantid,rep(i,ind[i]-ind[i-1]))
}
rndb2$participantid <- participantid

## then we can calculate participant * schema * round
rndb3 <- rndb2 %>% group_by(participantid,expl_phase,schemaID) %>%
  summarise(sumrt = sum(rt),payoff= mean(payoff))
### the simple way to do this, simply take means and variance
moyu <- rndb3 %>% group_by(schemaID) %>%
  summarise(explmean = mean(sumrt),explvar=var(sumrt))
####

## arrange participants' schema exploration time, get schema * round distribution
rndb3 <- rndb3 %>% group_by(participantid,schemaID) %>%
  arrange(participantid,schemaID,desc(sumrt))%>%
  mutate(rkround = row_number(desc(sumrt)))

## rescale the distribution, (a-min)/(max-min)
scaledrndb <- rndb3 %>% group_by(schemaID,rkround) %>%
  mutate(scaledrt = sumrt/100)

a=scaledrndb %>% filter(schemaID==1 & rkround==1)

## cal mean and var by schema and rank
rndb4 <- scaledrndb %>% group_by(schemaID,rkround) %>%
  summarise(meanr = mean(scaledrt), varr=var(scaledrt))
## filter participants with too many rounds
rndb4 <- rndb4 %>%
  filter(rkround<13)

## get the mean of mean, mean of variance for schema
rndb5 <- rndb4 %>% group_by(schemaID) %>%
  summarise(mean = mean(meanr,na.rm = T), var=mean(varr,na.rm = T))
## as we scaled the data, we can also change it back
noscaled <- rndb3 %>%
  filter(rkround<13) %>% group_by(schemaID,rkround) %>%
  summarise(meanr = mean(sumrt), varr=var(sumrt)) %>% 
  group_by(schemaID) %>%
  summarise(mean0 = mean(meanr,na.rm = T), var0 =mean(varr,na.rm = T))
final <- merge(rndb5,noscaled, by = 'schemaID') %>%
  mutate(modvar=var*mean0/mean) %>% select(c('schemaID','mean0','modvar'))


## tag payoff back
payoffdt <- expl %>% group_by(schemaID) %>% summarise(payoff = mean(payoff))
final$payoff <- payoffdt$payoff
## merge with familiarity data
schemainfo2 <- merge(fam,final,by.x = c('id','payoff'),by.y = c('schemaID','payoff'))
colnames(schemainfo2)[1] <- 'schemaID'

## plot results
ggplot(scaledrndb)+
  geom_histogram(aes(scaledrt))+
  facet_wrap(~schemaID)

ggplot(rndb4)+
  geom_histogram(aes(varr))+
  facet_wrap(~rkround)+
  ggtitle("distribution of the variance of round distribution")


## merge with familiarity data
write.csv(schemainfo2,file = 'data/new_exp/painting_schemainfo2.csv')
write.csv(schemainfo2, file = 'data/new_exp/quote_schemainfo2.csv')


###########
# try to get the total time
expl <- read.csv("data/new_exp/matfile/expP.csv")
dect <- read.csv("data/new_exp/matfile/decP.csv")
expl <- read.csv("data/new_exp/matfile/expQ.csv")
dect <- read.csv("data/new_exp/matfile/decQ.csv")
e2id <- expl %>% group_by(Group,ID) %>%
  summarise(total_expl = sum(rt),
            total_round = n_distinct(expl_phase),
            expl_per_round = total_expl/total_round) %>%
  arrange(ID)
dect <- dect %>% group_by(ID) %>%
  mutate(decphase = row_number())
d2id <- dect %>% group_by(Group,ID) %>%
  summarise(total_dec = sum(dectime),
            total_decphase = n_distinct(decphase),
            dec_per_decphase = total_dec/total_decphase)%>%arrange(ID)
total_time = merge(d2id,e2id,by = c("Group","ID"))
# total_time = total_time %>% mutate(total_rest = 180*(total_round-1),
#                     sumtime = total_dec+total_expl+total_rest)
total_time = total_time %>% mutate(sumtime = total_dec+total_expl)
#write.csv(total_time,'data/new_exp/processed/total_time_painting.csv')
write.csv(total_time,'data/new_exp/processed/total_time_quote.csv')

exptime = read.csv("data/new_exp/matfile/exptimestampP.csv")
firstexp = exptime %>% group_by(ID) %>% mutate(rk = row_number()) %>% filter(rk == 1)
f2 = exptime %>% group_by(ID) %>% filter(timestamp == min(timestamp))
lastdec = dect %>% group_by(ID) %>% filter(item4 == max(item4))

newsum = merge(firstexp,lastdec,by = c("Group","ID")) %>% 
  mutate(lastdec_minus_firstexp=item4-timestamp) %>%
  select(c("Group","ID","lastdec_minus_firstexp"))

total_time2 = merge(total_time,newsum,by = c("Group","ID"))
write.csv(total_time2,"data/new_exp/processed/total_time2_painting.csv",row.names = FALSE)

which(total_time2$sumtime - total_time2$lastdec_minus_firstexp>0)
