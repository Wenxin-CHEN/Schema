###### this file contains the code for model simulation #######

# import packages
library(tidyverse)
# set seed for random number generator
set.seed(12)

# create the empty data frames to store data
max_Round = 20
ALL.df = data.frame()
thres_item_inter.list <- c()
thres_item_final.list <- c()
thres_item.list <- c()
thres_schema.list <- c()
confidence.df <- data.frame()
gconfidence.df <- data.frame()
Time <- c()
for (i in 1:60) {
  temp <- rep(i,10)
  Time <- c(Time, temp)
}
#### initialize the agent's knowledge of the experimental structure, schema #####
a_schema = 0.2
h_schema = 1000
Beta_N = 0.2
Beta_Var = 0.3
a_generic = 0.1
h_generic = 1500
Beta_gN = 0.1
Beta_gVar = 0.2
w = 0.3
Phi = 20
decay_speed = 0.999
decay_speed_thres = 0.999
thres_item_inter = 6
thres_item_final = 14
thres_schema = 35 # check thres_schema, make sure it is not too higher
thres_schema_init = 40
theta_shift = 3
timevar = 0.0001
theta_shift_list = seq(5,40,1)
a_schema_list <- seq(a_schema*0.5, a_schema*1.5, a_schema*0.05)
h_schema_list <- seq(h_schema*0.5, h_schema*1.5, h_schema*0.05)
Beta_N_list <- seq(Beta_N*0.5, Beta_N*1.5, Beta_N*0.05) # learning rate is too high
Beta_N_list <- seq(0.1,0.3, by = 0.02)
Beta_Var_list <- seq(Beta_Var*0.5, Beta_Var*1.5, Beta_Var*0.05)
a_generic_list <- seq(a_generic*0.5, a_generic*1.5, a_generic*0.05)
h_generic_list <- seq(h_generic*0.5, h_generic*1.5, h_generic*0.05)
Beta_gN_list <- seq(Beta_gN*0.5, Beta_gN*1.5, Beta_gN*0.05)
Beta_gVar_list <- seq(Beta_gVar*0.5, Beta_gVar*1.5, Beta_gVar*0.05)
decay_speed_list <- seq(0.999 , 1, 0.00001)
decay_speed_thres_list <- seq(0.999 , 1, 0.000001)
thres_schema_list <- seq(15,28,0.15)
thres_item_final_list <- seq(4, 8, 0.1)
# timevar_list <- seq(0.0001,0.00011,0.0000003)
timevar_list <- seq()
Param.df <- data.frame(0, a_schema, h_schema, Beta_N, Beta_Var, a_generic, h_generic, 
                       Beta_gN, Beta_gVar, decay_speed, decay_speed_thres, thres_schema, thres_item_final)
colnames(Param.df) <- c("SubjectID", "a_schema", "h_schema", "Beta_N", "Beta_Var", "a_generic", "h_generic", 
                        "Beta_gN", "Beta_gVar", "decay_speed", "decay_speed_thres", "thres_schema",
                        "thres_item_final")


#### prepare functions for later computation ####

### sample a exploration time from the distribution, if it is negative, then re-sample
explengthsample <- function(expN,expVar){
  out = rnorm(1,expN,expVar)
  while (out <= 0)
    out = rnorm(1,expN,expVar)
  end
  out
}

### calculate the log function
log_inve <- function (x,a,h){
  if ((1/x)-1 > 0){
    out = ((log((1/x)-1)/-a) + h)
  }else if ((1/x)-1 < 0){
    out = ((-log(-((1/x)-1))/-a) + h)
  }else if ((1/x)-1 == 0){
    out = ((0/-a) + h)
  }
  out
}

### calculate the bonus based on the confidence
Bonus <- function (Con){
  if (Con >= 0.75){
    out = 3 * Con
  }else if(Con >= 0.25){
    out = 2 * Con
  }else{
    out = Con
  }
  out
}

## recovery status changes over time


################# Now run simulation for all parameter combinations ########

for (Subject in 1:length(timevar_list)) {
  #  a_schema <- sample(a_schema_list, size = 1, replace = T, prob = NULL)
  #  h_schema <- sample(h_schema_list, size = 1, replace = T, prob = NULL)
  #  Beta_N <- sample(Beta_N_list, size = 1, replace = T, prob = NULL)
  #  Beta_Var <- sample(Beta_Var_list, size = 1, replace = T, prob = NULL)
  #  a_generic <- sample(a_generic_list, size = 1, replace = T, prob = NULL)
  #  h_generic <- sample(h_generic_list, size = 1, replace = T, prob = NULL)
  #  Beta_gN <- sample(Beta_gN_list, size = 1, replace = T, prob = NULL)
  #  Beta_gVar <- sample(Beta_gVar_list, size = 1, replace = T, prob = NULL)
  #  decay_speed <- sample(decay_speed_list, size = 1, replace = T, prob = NULL)
  # Beta_N = Beta_N_list[Subject]
  #  decay_speed_thres <- sample(decay_speed_thres_list, size = 1, replace = T, prob = NULL)
  # thres_schema_init = thres_schema_list[Subject]
  # thres_item_final = thres_item_final_list[Subject]
  # thres_item_inter = thres_item_inter_list[Subject]
  # max_recovery = max_recovery_list[Subject]
  # theta_shift = theta_shift_list[Subject]
  timevar = timevar_list[Subject]
  Params <- c(Subject, a_schema, h_schema, Beta_N, Beta_Var, a_generic, h_generic, 
              Beta_gN, Beta_gVar, decay_speed, decay_speed_thres, thres_schema_init, thres_item_final)
  
  # instead of randomize the parameters to replicate the variance between individuals, we could simulate the model while
  # only one parameter value is varied. 
  
  
  Param.df <- rbind(Param.df, Params)
  
  ##### except for the schema selection part, other parts are the same as what we discussed
  #### create a data frame to store the outputs ####
  Outputs_cho = data.frame(Subject = c(rep(Subject, max_Round*2)), Round = c(1:(max_Round*2)), Schema = c(rep(0,max_Round*2)),Schema_RT = c(rep(0,max_Round*2)),
                           Schema_OB = c(rep(0,max_Round*2)),Schema_AS = c(rep(0,max_Round*2)),
                           Cho_1 = c(rep(0,max_Round*2)),Cho_2 = c(rep(0,max_Round*2)), Cho_3 = c(rep(0,max_Round*2)), 
                           Cho_4 = c(rep(0,max_Round*2)),RT_1 = c(rep(0,max_Round*2)), RT_2 = c(rep(0,max_Round*2)), 
                           RT_3 = c(rep(0,max_Round*2)), RT_4 = c(rep(0,max_Round*2)),OB_1 = c(rep(0,max_Round*2)), 
                           OB_2 = c(rep(0,max_Round*2)), OB_3 = c(rep(0,max_Round*2)), OB_4 = c(rep(0,max_Round*2)), 
                           AS_1 = c(rep(0,max_Round*2)), AS_2 = c(rep(0,max_Round*2)), AS_3 = c(rep(0,max_Round*2)), 
                           AS_4 = c(rep(0,max_Round*2)),
                           schema_payoff = c(rep(0,max_Round*2)), AC = c(rep(0,max_Round*2)), 
                           performance = c(rep(0,max_Round*2)))
  Outputs_learn = data.frame()
  Outputs_glearn = data.frame()
  
  
  ##### create the experiment for further simulation ######
  
  ### firstly, the presented item in exploration phase are generated ###
  schemainfo <- data.frame(schemaID = c(1:10), payoff = c(3, 4, 5, 2, 5, 6, 6, 2, 4, 3))
  
  # Expitem has a col for item's ID and a col for for schema's ID
  Expitem = data.frame(itemID = c(1:240))
  Expitem$SchemaID = (ceiling(Expitem$itemID/24))
  
  # Decitem has a col for item's ID, a col for schema's ID and a col for payoff
  Decitem = data.frame(ItemID = c(1:120))
  Decitem$SchemaID = (ceiling(Decitem$ItemID/12))
  Decitem$payoff = schemainfo$payoff[Decitem$SchemaID]
  
  # schemainfo contains conN (mean of confidence), conVar (variance of confidence), expN (mean of exploration time) and 
  # expVar (variance of exploration time)
  #schemainfo$conN = c(5,4,3,6,3,2,2,6,4,5)/6
  # 0.2 = 2, 0.35 = 3, 0.5 = 4, 0.65 = 5, 0.8 = 6
  schemainfo$conN = c(0.65, 0.5, 0.35, 0.8, 0.35, 0.2, 0.2, 0.8, 0.5, 0.65)
  #schemainfo$conN = rep(0,10)
  #schemainfo$conN = c(sample(seq(0,1/3,1/30),10, replace = TRUE, prob = NULL))
  #schemainfo$conN = c(sample(c(0,1/3,2/3,1),10, replace = TRUE, prob = NULL))
  # here we use the individual variance, so that the initial value of variance for each schema are the same
  schemainfo$conVar = c(rep(var(schemainfo$conN),10))
  ### the exploration length is sampled from the experimental data for each schema 
  schemainfo$expN = c(3.3681, 2.3268, 2.4154, 3.4589, 2.8481, 2.8867, 2.2546, 2.5310, 2.4061, 2.3860)
  schemainfo$expVar = c(53.6274, 91.8533, 47.8284, 107.0112, 73.0742, 131.9017, 59.3601, 174.0061, 83.4434, 18.1462)
  
  # initialize the generic confidence
  Gcon = data.frame(conN = mean(schemainfo$conN), conVar = mean(schemainfo$conVar))
  
  ##### let's start the experiment #####
  ThisRound = 1
  ##### when the agent has not finished the experiment, the loop continue as usual
  while (ThisRound <= max_Round){
    
    #### firstly, go through the exploration phase 
    ## firstly, sample the confidence prior the exploration
    Con_PriorExp = mapply(rnorm, 1, mean = schemainfo$conN, sd = schemainfo$conVar)
    
    ### we then sample the exploration length
    Exptime = mapply(explengthsample, expN = schemainfo$expN, expVar = schemainfo$expVar)
    
    Outputs_learn = rbind(Outputs_learn,schemainfo)
    Outputs_glearn = rbind(Outputs_glearn,Gcon)
    
    ### calculate the prior learning progress based on a inverse formula 
    learning = mapply(log_inve, Con_PriorExp, a_schema, h_schema)
    learning = learning + Exptime
    Con_afterExp = 1/(1+exp(-a_schema*(learning-h_schema)))
    schemainfo$conN = schemainfo$conN + Beta_N*(Con_afterExp - Con_PriorExp)
    schemainfo$conVar = schemainfo$conVar + Beta_Var*(abs(Con_afterExp - Con_PriorExp) - schemainfo$conVar)
    
    ### update the generic confidence
    Con_PriorExp = rnorm(1, mean = Gcon$conN, sd = Gcon$conVar)
    
    ### calculate the prior learning progress based on a inverse formula 
    learning = mapply(log_inve, Con_PriorExp, a_generic, h_generic)
    learning = learning + mean(Exptime)
    Con_afterExp = 1/(1+exp(-a_generic*(learning-h_generic)))
    Gcon$conN = Gcon$conN + Beta_gN*(Con_afterExp - Con_PriorExp)
    Gcon$conVar = Gcon$conVar + Beta_gVar*(abs(Con_afterExp - Con_PriorExp) - Gcon$conVar)
    Thischosen = schemainfo %>% group_by(payoff) %>% summarise(chosen = sample(schemaID,size = 1, prob = NULL))  
    ##### and we then move to the decision phase ######
    decisiontime = 1
    while (decisiontime <= 2){
      ThisPhase = ifelse(decisiontime == 1, (ThisRound*2)-1, ThisRound*2)
      #### again, before the decision making, we sample the schema-based and generic confidence
      Schema_EI = data.frame(schemaID = schemainfo$schemaID,payoff = schemainfo$payoff)
      Schema_EI$Scon = mapply(rnorm, 1, mean = schemainfo$conN, sd = schemainfo$conVar)
      Schema_EI$Gcon = rnorm(1, mean = Gcon$conN, sd = Gcon$conVar)
      
      #### firstly, we calculate the DMs for each schema
      Schema_EI$weightCon = w*Schema_EI$Scon + (1-w)*Schema_EI$Gcon
      Schema_EI$DM = mapply(Bonus,Schema_EI$weightCon) * Schema_EI$payoff
      
      #### we then determine which schemas will be presented in this screen
      if (decisiontime == 1){
        ChoSchema = Schema_EI[Schema_EI$schemaID %in% Thischosen$chosen,]
      }else if(decisiontime == 2){
        ChoSchema = Schema_EI[!Schema_EI$schemaID %in% Thischosen$chosen,]
      }
      
      ChoSchema$evidence = 0
      ChoItem = Decitem[Decitem$SchemaID %in% ChoSchema$schemaID,] %>% group_by(SchemaID) %>%
        summarise(chosen = sample(ItemID,size = 4, prob = NULL))
      
      #### firstly we prepare a table for evidence integration
      Item_EI = data.frame(ID = Decitem[Decitem$ItemID %in% ChoItem$chosen,1], 
                           Schema = Decitem[Decitem$ItemID %in% ChoItem$chosen,2], 
                           payoff = Decitem[Decitem$ItemID %in% ChoItem$chosen,3])
      Item_EI = Item_EI %>% group_by(Schema) %>% mutate(N = schemainfo$conN[schemainfo$schemaID == Schema], 
                                                        Var = schemainfo$conVar[schemainfo$schemaID == Schema],
                                                        DM = ChoSchema$DM[Schema == ChoSchema$schemaID])
      Item_EI$evidence = 0 # evidence integration
      Item_EI$recovery = 0
      Item_EI$timevar = 1
      Item_EI$decision = 0
      Item_EI$OB = 0
      Item_EI$AS = 0
      Item_EI$status = 0 # mark the status of item's threshold, 0:inter; 1:final
      Item_EI$thres = thres_item_inter
      #### let's start the evidence integration ####
      schema_decision = 0
      Finish = 0
      attention = 0
      shift = 0
      CTime = 0
      thres_schema = thres_schema_init
      while (Finish <= 4){
        
        if (attention == 0) {### i.e., when decision has not been made and no item were attended
          Item_EI$timevar = 1-(1/(exp(timevar*Item_EI$recovery)))
          if (sum(Item_EI$decision == 0) == 1){
            attention = Item_EI$ID[Item_EI$decision == 1]
          }else{
            if (schema_decision == 0){
              p.list = (exp(Phi*Item_EI$evidence[Item_EI$decision == 0] *Item_EI$DM[Item_EI$decision == 0] * Item_EI$timevar[Item_EI$decision == 0]))/
                sum((exp(Phi*Item_EI$evidence[Item_EI$decision == 0] *Item_EI$DM[Item_EI$decision == 0] * Item_EI$timevar[Item_EI$decision == 0])))
              p.list[is.na(p.list)] <- 1
              attention = sample(Item_EI$ID[Item_EI$decision == 0],1,
                                 prob = p.list)
            } else{
              p.list <- (exp(Phi*Item_EI$evidence[Item_EI$decision == 0]* Item_EI$timevar[Item_EI$decision == 0]))/
                sum((exp(Phi*Item_EI$evidence[Item_EI$decision == 0]* Item_EI$timevar[Item_EI$decision == 0])))
              p.list[is.na(p.list)] <- 1
              attention = sample(Item_EI$ID[Item_EI$decision == 0],1,
                                 prob = p.list)
            }
          }
          Item_EI$OB[Item_EI$ID == attention] = 1
          Item_EI$AS[Item_EI$ID == attention] = Item_EI$AS[Item_EI$ID == attention] + 1
          shift = 0
        }
        
        while (shift == 0) {### i.e., when the attention does not shift
          ### evidence integration
          CTime = CTime + 1
          Item_EI$evidence[Item_EI$ID == attention] = Item_EI$evidence[Item_EI$ID == attention] +
            rnorm(1,Item_EI$N[Item_EI$ID == attention], Item_EI$Var[Item_EI$ID == attention])
          
          Item_EI$recovery[Item_EI$ID != attention] = Item_EI$recovery[Item_EI$ID != attention] + 1
          
          ### evidence decay
          # evidence decay as time passes
          Item_EI$evidence[Item_EI$ID != attention & Item_EI$decision == 0] = 
            Item_EI$evidence[Item_EI$ID != attention & Item_EI$decision == 0] *decay_speed
          # threshold decay as time passes
          Item_EI$thres = Item_EI$thres * decay_speed_thres
          thres_schema = thres_schema * decay_speed_thres
          Item_EI$evidence[Item_EI$evidence > Item_EI$thres] = Item_EI$thres
          
          ### identification completed, once the threshold is reached
          if (any(Item_EI$evidence[Item_EI$decision == 0 & Item_EI$ID == attention] >= Item_EI$thres[Item_EI$decision == 0 & Item_EI$ID == attention])){
            shift = 1 # attention shift

            if (schema_decision != 0){
              for (i in 1:length(Item_EI$ID[Item_EI$evidence >= Item_EI$thres &Item_EI$decision == 0 & Item_EI$ID == attention])) {
                Outputs_cho[ThisPhase,6+Finish] = Item_EI$ID[Item_EI$evidence >= Item_EI$thres &Item_EI$decision == 0][[i]]
                Outputs_cho[ThisPhase,10+Finish] = CTime
                Outputs_cho[ThisPhase,14+Finish] = sum(Item_EI$OB == 1)
                Outputs_cho[ThisPhase,18+Finish] = sum(Item_EI$AS)
                Item_EI$OB = 0
                Item_EI$AS = 0
                CTime = 0
                Finish = Finish + 1
              }
            }
            Item_EI$decision[Item_EI$evidence >= Item_EI$thres & Item_EI$decision == 0 & Item_EI$status == 1] = 1 # mark the decision as 1
            Item_EI$thres[Item_EI$evidence >= Item_EI$thres & Item_EI$decision == 0 & Item_EI$status == 0] = thres_item_final
            Item_EI$status[Item_EI$thres == thres_item_final & Item_EI$decision == 0 & Item_EI$status == 0] = 1
          }else{
            shift = sample(c(1,rep(0,theta_shift)),1, prob = NULL) # attention might shift randomly
          }
          ### summarize the evidence for schema
          
          if (schema_decision == 0){
            ChoSchema = ChoSchema %>% group_by(schemaID) %>% mutate(evidence = sum(Item_EI$evidence[Item_EI$Schema == schemaID]))
            if (any(ChoSchema$evidence >= thres_schema)){
              if (length(ChoSchema$schemaID[ChoSchema$evidence >= thres_schema]) != 1){
                schema_decision =  ChoSchema$schemaID[ChoSchema$evidence == max(ChoSchema$evidence)]
              }else{
                schema_decision = ChoSchema$schemaID[ChoSchema$evidence >= thres_schema]
              }
              Item_EI$evidence[Item_EI$Schema != schema_decision] = 0
              #thres_item - Item_EI$evidence[Item_EI$Schema != schema_decision]
              Item_EI$N[Item_EI$Schema != schema_decision] = 1 - Item_EI$N[Item_EI$Schema != schema_decision]
              Outputs_cho$Schema[ThisPhase] = schema_decision
              Outputs_cho$Schema_RT[ThisPhase] = CTime
              Outputs_cho$Schema_OB[ThisPhase] = sum(Item_EI$OB == 1)
              Outputs_cho$Schema_AS[ThisPhase] = sum(Item_EI$AS)
              Outputs_cho$schema_payoff[ThisPhase] = ChoSchema$payoff[ChoSchema$schemaID == schema_decision]
              Item_EI$OB = 0
              Item_EI$AS = 0
              CTime = 0
              shift = 1
              Item_EI$decision = 0
              Finish = 1
              Item_EI$thres[Item_EI$status == 0] = thres_item_final
            }
          }
          
          if (shift == 1){
            # once the attention shift, we mark the last item
            Item_EI$recovery[Item_EI$ID == attention] = 0
            Item_EI$timevar[Item_EI$ID == attention] = 0
            attention = 0
          }
        }
      }
      ##### deliver the feedback
      Schema_res = Item_EI$Schema[Item_EI$ID %in% Outputs_cho[ThisPhase,7:10]]
      if (length(unique(Schema_res)) == 1){
        Outputs_cho$AC[ThisPhase] = 1
        Outputs_cho$performance[ThisPhase] = sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,7:10]]) * 3
      }else if(length(unique(Schema_res)) == 2 & sum(Schema_res == unique(Schema_res)[1]) != 2){
        Outputs_cho$AC[ThisPhase] = 0.5
        Outputs_cho$performance[ThisPhase] = sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,7:10]]) * 2
      }else{
        Outputs_cho$AC[ThisPhase] = 0
        Outputs_cho$performance[ThisPhase] = sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,7:10]])
      }
      
      Outputs_learn = rbind(Outputs_learn,schemainfo)
      Outputs_glearn = rbind(Outputs_glearn,Gcon)
      
      schemainfo$conN[schemainfo$schemaID %in% unique(Schema_res)] = 
        schemainfo$conN[schemainfo$schemaID %in% unique(Schema_res)] + 
        Beta_N*(Outputs_cho$AC[ThisPhase] - Schema_EI$Scon[Schema_EI$schemaID %in% unique(Schema_res)])
      
      schemainfo$conVar[schemainfo$schemaID %in% unique(Schema_res)] = 
        schemainfo$conVar[schemainfo$schemaID %in% unique(Schema_res)] + 
        Beta_Var*(abs(Schema_EI$Scon[Schema_EI$schemaID %in% unique(Schema_res)] - Outputs_cho$AC[ThisPhase]) - 
                    schemainfo$conVar[schemainfo$schemaID %in% unique(Schema_res)])
      
      Gcon$conN = Gcon$conN + Beta_gN*(Outputs_cho$AC[ThisPhase] - unique(Schema_EI$Gcon))
      Gcon$conVar = Gcon$conVar + Beta_gVar*(abs(Outputs_cho$AC[ThisPhase] - unique(Schema_EI$Gcon)) - Gcon$conVar)
      decisiontime = decisiontime + 1
    }
    ThisRound = ThisRound + 1
  }
  ALL.df <- rbind(ALL.df, Outputs_cho)
  Outputs_learn <- cbind(Outputs_learn, Time, rep(Subject, nrow(Outputs_learn)))
  Outputs_glearn <- cbind(Outputs_glearn, rep(Subject, nrow(Outputs_glearn)))
  confidence.df <- rbind(confidence.df, Outputs_learn)
  gconfidence.df <- rbind(gconfidence.df, Outputs_glearn)
  
}

# write_csv(ALL.df,'allresult.csv')
# write_csv(Param.df, 'Paras.csv')
# write_csv(confidence.df, 'confidence.csv')
# write_csv(gconfidence.df, 'gconfidence.csv')
