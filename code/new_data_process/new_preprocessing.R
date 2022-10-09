library(tidyverse)
preprocess<-function(dt){
exp <- dt
colnames(exp) <- c("ID_left","round_left","decphase","Timestamp",
                   "choice","length","PicID","schema","novelty",
                   "ID_right","performance","accuracy","payoff")

# drop row with na
exp <- exp[complete.cases(exp),]

## reaction time
spreview <- exp %>% 
  group_by(ID_left,round_left,decphase,choice) %>%
  arrange(Timestamp, .by_group = TRUE) %>%
  summarise(sumtime=sum(length),payoff=mean(payoff),
            performance=mean(performance),accuracy=mean(accuracy))
## novelty, payoff and accuracy
payac <- exp %>%
  group_by(ID_left,round_left,decphase) %>%
  summarise(novelty=mean(novelty),payoff=mean(payoff),
            performance=mean(performance),
            accuracy=mean(accuracy))

## find the last view pic to find AS
asoul <- exp %>% filter(PicID!=0)%>%
  group_by(ID_left,round_left,decphase,choice) %>%
  arrange(Timestamp, .by_group = TRUE)%>%
  mutate(last_view = lag(PicID))%>%
  mutate(as = (last_view != PicID))


assql2 <- asoul %>% 
  dplyr::select(ID_left,round_left,decphase,choice,
                PicID,length,schema,last_view,as) %>%
  replace(is.na(.), 0) %>%
  group_by(ID_left,round_left,decphase,choice) %>%
  mutate(chosen_Pic=last(PicID),chosen_schema = last(schema)) %>%
  summarise(chosen_Pic=mean(chosen_Pic),chosen_schema=mean(chosen_schema),
            as_num = sum(as,na.rm = T),
            ob_num = n_distinct(PicID),
            dw_mean = sum(length)/(sum(as)+1))


# get ob,as,dw at once
spreview_wideobasdw <- assql2 %>%
    pivot_wider(names_from = choice, names_sep = "_",
                values_from = c(ob_num,as_num,dw_mean,
                                chosen_Pic,chosen_schema)) %>%
    na.omit()



# change the colname
newcolname <- c(sapply(c("OB_","AS_","DW_"),
                       function(var1,var2){paste0(var1,var2)},
                       var2 = 1:4))
colnames(spreview_wideobasdw)[4:15] = newcolname

# get the reaction time, which is from another sheet
spreview_time <- spreview %>%
  dplyr::select(ID_left,round_left,decphase,choice,sumtime) %>%
  pivot_wider(names_from = choice,values_from=sumtime,
              names_glue = "RT_{choice}") %>%
  na.omit()

expall <- spreview_wideobasdw %>%
  left_join(spreview_time, by = c('ID_left'='ID_left','round_left'='round_left','decphase'='decphase')) %>%
  left_join(payac, by = c('ID_left'='ID_left','round_left'='round_left','decphase'='decphase'))
## extract the group (L,H,Lc,Hc,LH,HL)
expall$group <- str_extract(expall$ID_left,"[a-zA-Z]+")
expall <- expall %>% relocate(chosen_Pic_1:chosen_schema_4,.after=last_col())

return(expall)
}

rtp <- read.csv("data/new_exp/allP.csv",header = FALSE)
rtq <- read.csv("data/new_exp/allQ1.csv",header = FALSE)
painting <- preprocess(rtp)
quote <- preprocess(rtq)
write.csv(painting,file = "data/new_exp/prep_painting.csv")
write.csv(quote,file = "data/new_exp/prep_quote.csv")
exp %>% group_by(ID_left) %>% 
  summarise(count(schema))
