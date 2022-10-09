rtp <- read.csv("data/new_exp/allP.csv",header = FALSE)
rtq <- read.csv("data/new_exp/allQ1.csv",header = FALSE)


exp <- rtp
colnames(exp) <- c("ID_left","round_left","decphase","Timestamp",
                   "choice","length","PicID","schema","novelty",
                   "ID_right","performance","accuracy","payoff")

### plot to find changed schema
exp$group <- str_extract(exp$ID_left,"[a-zA-Z]+")
changeschema <- exp %>% filter(group %in% c("H","L"), schema!=0) %>%
  group_by(group) %>% 
  count(schema)
ggplot(changeschema) +
  geom_bar(aes(x=schema,y=n,fill=group),
           position="dodge",stat="identity")


exp1<- exp %>% na.omit() %>%
  group_by(ID_left) %>% summarise(total_time = sum(length)/1000,
                                  total_round = n_distinct(round_left),
                                  round_time = total_time/total_round)
write.csv(exp1,"data/new_exp/processed/painting_expl.csv",row.names = FALSE)
write.csv(exp1,"data/new_exp/processed/quote_expl.csv",row.names = FALSE)
