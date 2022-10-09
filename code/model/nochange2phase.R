###### this file contains the code for model simulation #######

# import packages
library(tidyverse)

### decide the type of experiment, and this should be run first
exp_type = 'painting'
if (exp_type == 'painting'){
  expschema = read.csv("data/new_exp/painting_schemainfo2.csv")
} else {
  expschema = read.csv('data/new_exp/quote_schemainfo2.csv')
}

run_model <- function(run_name,para_name, para_list,type="L"){
  #### run_name: used to create the data fold to save results
  #### para_name: name of para (such as timevar); 
  #### para_list: list of values of that estimating para;
  #### type: include novelty and risk info, can be
  #### L; LH; H; HL; Lc; Hc default is L
  ###### list of para_name:
  # Subject, a_schema, h_schema, Beta_N, Beta_Var, a_generic, 
  # h_generic, Beta_gN, Beta_gVar, decay_speed,
  # decay_speed_thres, thres_schema, thres_item_final
  # thres_item_inter, max_recovery, theta_shift, timevar
  
  ###### NOTICE: we will create a directory named data to save results 
  
  ## try the function first
  # run_name = 'timetry'
  # para_name = "thres_schema_init"
  # para_list = c(40)
  # novelty: 1 -- 5 new schemas; 0 -- schema remains the same

 
  ######## create a place to save data
  if (!dir.exists(file.path("data",run_name))){
    dir.create(file.path("data",run_name),recursive = T)
  }
  
  # create the empty data frames to store data
  ALL.df = data.frame()
  confidence.df <- data.frame()
  gconfidence.df <- data.frame()
  ###dwell_all_subject saves the dwell time of all subjects per round
  dwell_all_subject = data.frame()
  
  #### initialize the agent's knowledge of the experimental structure, schema #####
  ### ppt11
  a_schema = 0.2
  h_schema = 1000
  Beta_N = 0.2
  Beta_Var = 0.3
  a_generic = 0.1
  h_generic = 1500
  Beta_gN = 0.1
  Beta_gVar = 0.2
  w = 0.3
  Phi = 5
  decay_speed = 0.999
  decay_speed_thres = 0.999 # decay of item threshold
  thres_item_inter = 15
  thres_item_final = 20
  thres_schema = 35 # check thres_schema, make sure it is not too higher
  thres_schema_init = 40
  theta_shift = 3
  timevar = 0.00001
  modeltimestamp = 0.061
  
  ### lhy
  # a_schema = 0.2
  # h_schema = 1000
  # Beta_N = 0.2
  # Beta_Var = 0.3
  # a_generic = 0.1
  # h_generic = 1500
  # Beta_gN = 0.1
  # Beta_gVar = 0.2
  # w = 0.3
  # Phi = 20
  # decay_speed = 0.999
  # decay_speed_thres = 0.999
  # thres_item_inter = 6
  # thres_item_final = 14
  # thres_schema = 40 # check thres_schema, make sure it is not too higher
  # theta_shift = 3
  # timevar = 0.0001
  # modeltimestamp = 0.061
  
  Param.df <- data.frame(0, a_schema, h_schema, Beta_N, Beta_Var, a_generic, h_generic, 
                         Beta_gN, Beta_gVar, decay_speed, decay_speed_thres, thres_schema,
                         thres_item_inter, thres_item_final, theta_shift, timevar, modeltimestamp)
  colnames(Param.df) <- c("SubjectID", "a_schema", "h_schema", "Beta_N", "Beta_Var", "a_generic", "h_generic", 
                          "Beta_gN", "Beta_gVar", "decay_speed", "decay_speed_thres", "thres_schema",
                          "thres_item_inter","thres_item_final","theta_shift","timevar", "modeltimestamp")


  
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
  
  for (Subject in 1:length(para_list)) {
    # instead of randomize the parameters to replicate the variance 
    # between individuals, we could simulate the model while
    # only one parameter value is varied. 
    assign(para_name,para_list[Subject])
    
    ## save the params used for subject
    Params <- c(Subject, a_schema, h_schema, Beta_N, Beta_Var, a_generic, h_generic, 
                Beta_gN, Beta_gVar, decay_speed, decay_speed_thres, thres_schema, 
                thres_item_inter, thres_item_final, theta_shift, timevar)
    Param.df <- rbind(Param.df, Params)
    
    ##### except for the schema selection part, other parts are the same as what we discussed
    #### create a data frame to store the outputs ####
    max_Round = 200
    ### here we generate the result of one subject
    Outputs_cho = data.frame(Subject = c(rep(Subject, max_Round*2)), Round = c(rep(1:(max_Round*2),each=2)),
                             Schema = c(rep(0,max_Round*2)),Schema_RT = c(rep(0,max_Round*2)),
                             Schema_OB = c(rep(0,max_Round*2)),Schema_AS = c(rep(0,max_Round*2)),
                             Cho_1 = c(rep(0,max_Round*2)),Cho_2 = c(rep(0,max_Round*2)), Cho_3 = c(rep(0,max_Round*2)), 
                             Cho_4 = c(rep(0,max_Round*2)),RT_1 = c(rep(0,max_Round*2)), RT_2 = c(rep(0,max_Round*2)), 
                             RT_3 = c(rep(0,max_Round*2)), RT_4 = c(rep(0,max_Round*2)),OB_1 = c(rep(0,max_Round*2)), 
                             OB_2 = c(rep(0,max_Round*2)), OB_3 = c(rep(0,max_Round*2)), OB_4 = c(rep(0,max_Round*2)), 
                             AS_1 = c(rep(0,max_Round*2)), AS_2 = c(rep(0,max_Round*2)), AS_3 = c(rep(0,max_Round*2)), 
                             AS_4 = c(rep(0,max_Round*2)),
                             schema_payoff = c(rep(0,max_Round*2)), AC = c(rep(0,max_Round*2)), 
                             performance = c(rep(0,max_Round*2)),Phase = c(1:(max_Round*2)))
    Outputs_learn = data.frame()
    Outputs_glearn = data.frame()
    dwell_table = data.frame() # before break 0, afterbreak =1
    
    
    ##### create the experiment for further simulation ######
    ### firstly, schema info is based on exp results ###

    
    # schemainfo contains conN (mean of confidence), 
    # conVar (variance of confidence), expN (mean of exploration time) and 
    # expVar (variance of exploration time) 
    # schemainfo <- data.frame(schemaID = c(1:10), payoff = c(3, 4, 5, 2, 5, 6, 6, 2, 4, 3))
    # schemainfo$novelty = FALSE ## at first, no new schema, so the novelty is False
    # # 0.2 = 2, 0.35 = 3, 0.5 = 4, 0.65 = 5, 0.8 = 6
    # schemainfo$conN = c(0.65, 0.5, 0.35, 0.8, 0.35, 0.2, 0.2, 0.8, 0.5, 0.65)
    # # here we use the individual variance, so that the initial value of variance for each schema are the same
    # schemainfo$conVar = c(rep(var(schemainfo$conN),10))
    # ### the exploration length is sampled from the experimental data for each schema 
    # #schemainfo$expN = c(3.3681, 2.3268, 2.4154, 3.4589, 2.8481, 2.8867, 2.2546, 2.5310, 2.4061, 2.3860)
    # #schemainfo$expVar = c(53.6274, 91.8533, 47.8284, 107.0112, 73.0742, 131.9017, 59.3601, 174.0061, 83.4434, 18.1462)
    # ## use new exploratime time
    # schemainfo2 <- expschema %>% filter(new==0)
    # schemainfo$expN = schemainfo2$mean0
    # schemainfo$expVar = schemainfo2$modvar
    
    ### firstly, the presented item in exploration phase are generated ###
    # schemainfo contains conN (mean of confidence), conVar (variance of confidence), expN (mean of exploration time) and 
    # expVar (variance of exploration time)
    schemainfo <- expschema  %>% filter(new==0) %>% select(-c(author,new))
    colnames(schemainfo)[c(3,5,6)] = c('conN','expN','expVar')
    ## reset familiarity 
    schemainfo = arrange(schemainfo,desc(payoff))
    schemainfo$conN = c(0.2,0.2,0.35,0.35,0.5,0.5,0.65,0.65,0.8,0.8)
    ### the exploration length is sampled from the experimental data for each schema 
    schemainfo$expN = c(3.3681, 2.3268, 2.4154, 3.4589, 2.8481, 2.8867, 2.2546, 2.5310, 2.4061, 2.3860)
    schemainfo$expVar = c(53.6274, 91.8533, 47.8284, 107.0112, 73.0742, 131.9017, 59.3601, 174.0061, 83.4434, 18.1462)
    # here we use the individual variance, so that the initial value of variance for each schema are the same
    schemainfo$conVar = c(rep(var(schemainfo$conN),10))
    schemainfo$novelty = FALSE ## at first, no new schema, so the novelty is False
    
    ## used for schemacount
    schemainfo$afterbreak = 0
    schemainfo$round = 0
    
    ### Secondly, the presented item in exploration phase are generated ###
    # Expitem has a col for item's ID and a col for for schema's ID
    Expitem = data.frame(itemID = c(1:240))
    Expitem$SchemaID = rep(schemainfo$schemaID,each=24)
    
    # Decitem has a col for item's ID, a col for schema's ID and a col for payoff
    Decitem = data.frame(ItemID = c(1:120))
    Decitem$SchemaID = rep(schemainfo$schemaID,each=12)
    Decitem$payoff = rep(schemainfo$payoff,each=12)
    
    # initialize the generic confidence
    Gcon = data.frame(conN = mean(schemainfo$conN), conVar = mean(schemainfo$conVar))
    
    ##### let's start the experiment #####
    timecounter = 0
    ## for first section (before condition change)
    # first exploration phase is 10 min
    ## firstly, sample the confidence prior the exploration
    Con_PriorExp = mapply(rnorm, 1, mean = schemainfo$conN, sd = schemainfo$conVar)
    ### we then sample the exploration length
    Exptime = mapply(explengthsample, expN = schemainfo$expN, expVar = schemainfo$expVar)
    Exptime = Exptime  * 600/sum(Exptime) # rescale time to 10 min
    timecounter = timecounter + 600 

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
    
    
    ##### after first 10 min exploration, start exploration
    ThisRound = 1
    while (TRUE){
      ##### start from the decision phase ######
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
        Finish = 0 # 0 before schema selected; 1234 for selections
        attention = 0
        shift = 0
        dwelltime= 0 # rndb
        dwellshift = 0 #rndb, use dwellshift to see whether the choice is changed; tell differnece between choice 0 and 1234
        CTime = 0
        print(Subject)
        print(ThisPhase) #### hhhhhh
        print(para_name)
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
            CTime = CTime + modeltimestamp
            dwelltime = dwelltime + modeltimestamp
            timecounter = timecounter + modeltimestamp
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
                  dwellshift = 1 # rndb change choice
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
                dwellshift = 1 # rndb change from choice 0 to choice 1
                Item_EI$thres[Item_EI$status == 0] = thres_item_final
              }
            }
            
            if (shift == 1){
              # once the attention shift, we mark the last item
              Item_EI$recovery[Item_EI$ID == attention] = 0
              Item_EI$timevar[Item_EI$ID == attention] = 0
              
              ### attention shift, record the dwell time
              onerecord = data.frame(Subject=Subject,Round=ThisRound,Phase=ThisPhase,
                                     choice=Finish-dwellshift,
                                     PicID=attention,
                                     dwelltime=dwelltime,afterbreak=0
                                     )
              dwell_table = rbind(dwell_table,onerecord)
              dwelltime = 0
              dwellshift = 0 # set dwellshift to 0 again
              attention = 0
            }
          }
        }
        ##### deliver the feedback
        Schema_res = Item_EI$Schema[Item_EI$ID %in% Outputs_cho[ThisPhase,7:10]]
        if (type %in% c("H","HL","Hc")){ # high risk before break
          if (length(unique(Schema_res)) == 1){ # 4/4
            Outputs_cho$AC[ThisPhase] = 1
            Outputs_cho$performance[ThisPhase] = sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,7:10]]) * 3
          }else if(length(unique(Schema_res)) == 2 & sum(Schema_res == unique(Schema_res)[1]) != 2){
            # 3/4
            Outputs_cho$AC[ThisPhase] = 0.5
            Outputs_cho$performance[ThisPhase] = sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,7:10]])
          }else{
            Outputs_cho$AC[ThisPhase] = 0
            Outputs_cho$performance[ThisPhase] = sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,7:10]])
          }
        }else{ # low risk
          if (length(unique(Schema_res)) == 1){ #4/4
            Outputs_cho$AC[ThisPhase] = 1
            Outputs_cho$performance[ThisPhase] = sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,7:10]]) * 3
            }else if(length(unique(Schema_res)) == 2 & sum(Schema_res == unique(Schema_res)[1]) != 2){
            # 3/4
            Outputs_cho$AC[ThisPhase] = 0.5
            right_schema =  as.data.frame(sort(table(Schema_res),decreasing=TRUE))[1,1] # most frequent schema chosen
            Outputs_cho$performance[ThisPhase] = sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,7:10]])  +  schemainfo$payoff[schemainfo$schemaID==right_schema]*3
            }else{
            Outputs_cho$AC[ThisPhase] = 0
            Outputs_cho$performance[ThisPhase] = sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,7:10]])
            }
          }
        schemainfo$afterbreak = 0 # schemacount test
        schemainfo$round = ThisPhase
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
      }# the end of while (decisiontime <= 2)
      ThisRound = ThisRound + 1
      ### check for time
      if (timecounter >= 1500){ # after (35-10)*60=1500
        print(paste0("first timeout after decision:",timecounter))
        break}
      
      ### then the exploration phase turn to be 3 min 
      ## firstly, sample the confidence prior the exploration
      Con_PriorExp = mapply(rnorm, 1, mean = schemainfo$conN, sd = schemainfo$conVar)
      ### we then sample the exploration length
      Exptime = mapply(explengthsample, expN = schemainfo$expN, expVar = schemainfo$expVar)
      Exptime = Exptime  * 180/sum(Exptime) # rescale time to 3 min
      timecounter = timecounter + 180 
      
      schemainfo$afterbreak = 0 # schemacount test
      schemainfo$round = ThisPhase
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
    } #  the end of while (TRUE), before break ends
    
    
    Outputs_cho = Outputs_cho[1:ThisPhase,]
    Outputs_cho$novelty = 0 
    breakR = ThisRound-1 ## record the round when before break phase end
    Outputs_cho$afterbreak = 0 # before break
    Outputs_cho$breakR = breakR ## add the break round
    ALL.df <- rbind(ALL.df, Outputs_cho)
    print("before break ok")
    
    
    #### after 35 min, break takes place and condition may change
    timecounter = timecounter + 300 # break is 5 min
    
    ### if the novelty included, 5 schema will be replaced
    if (type %in% c("H","L")){
      schemainfo2 <- expschema  %>% filter(type=='new') %>% select(-c(author,new))
      colnames(schemainfo2)[c(3,5,6)] = c('conN','expN','expVar')
      ## reset familiarity 
      schemainfo2 = arrange(schemainfo2,desc(payoff))
      schemainfo2$conN = c(0.2,0.35,0.5,0.65,0.8)
      # here we use the individual variance, so that the initial value of variance for each schema are the same
      schemainfo2$conVar = c(rep(var(schemainfo2$conN),5))
      schemainfo2$novelty = TRUE 
      # combine with the same (unchanged) schema
      schemainfo <- schemainfo %>% filter(type=='same')
      schemainfo2$afterbreak = 1 # schemacount test
      schemainfo2$round = 0
      schemainfo <- rbind(schemainfo,schemainfo2)
      schemainfo$round = 0
      
      ### the exploration length is sampled from the experimental data for each schema 
      schemainfo$expN = c(3.3681, 2.3268, 2.4154, 3.4589, 2.8481, 2.8867, 2.2546, 2.5310, 2.4061, 2.3860)
      schemainfo$expVar = c(53.6274, 91.8533, 47.8284, 107.0112, 73.0742, 131.9017, 59.3601, 174.0061, 83.4434, 18.1462)
      
      ### Secondly, the presented item in exploration phase are generated ###
      # Expitem has a col for item's ID and a col for for schema's ID
      Expitem = data.frame(itemID = c(1:240))
      Expitem$SchemaID = rep(schemainfo$schemaID,each=24)
      
      # Decitem has a col for item's ID, a col for schema's ID and a col for payoff
      Decitem = data.frame(ItemID = c(1:120))
      Decitem$SchemaID = rep(schemainfo$schemaID,each=12)
      Decitem$payoff = rep(schemainfo$payoff,each=12)
      
      # initialize the generic confidence
      Gcon = data.frame(conN = mean(schemainfo$conN), conVar = mean(schemainfo$conVar))
      print("novelty change")}
    
    ### first exploration phase after break is 5 min
    
    #### create a data frame to store the outputs again ####
    max_Round = 200
    Outputs_cho = data.frame(Subject = c(rep(Subject, max_Round*2)), Round = c(rep(1:(max_Round*2),each=2)),
                             Schema = c(rep(0,max_Round*2)),Schema_RT = c(rep(0,max_Round*2)),
                             Schema_OB = c(rep(0,max_Round*2)),Schema_AS = c(rep(0,max_Round*2)),
                             Cho_1 = c(rep(0,max_Round*2)),Cho_2 = c(rep(0,max_Round*2)), Cho_3 = c(rep(0,max_Round*2)), 
                             Cho_4 = c(rep(0,max_Round*2)),RT_1 = c(rep(0,max_Round*2)), RT_2 = c(rep(0,max_Round*2)), 
                             RT_3 = c(rep(0,max_Round*2)), RT_4 = c(rep(0,max_Round*2)),OB_1 = c(rep(0,max_Round*2)), 
                             OB_2 = c(rep(0,max_Round*2)), OB_3 = c(rep(0,max_Round*2)), OB_4 = c(rep(0,max_Round*2)), 
                             AS_1 = c(rep(0,max_Round*2)), AS_2 = c(rep(0,max_Round*2)), AS_3 = c(rep(0,max_Round*2)), 
                             AS_4 = c(rep(0,max_Round*2)),
                             schema_payoff = c(rep(0,max_Round*2)), AC = c(rep(0,max_Round*2)), 
                             performance = c(rep(0,max_Round*2)),Phase = c(1:(max_Round*2)))
    
    
    ## firstly, sample the confidence prior the exploration
    Con_PriorExp = mapply(rnorm, 1, mean = schemainfo$conN, sd = schemainfo$conVar)
    ### we then sample the exploration length
    Exptime = mapply(explengthsample, expN = schemainfo$expN, expVar = schemainfo$expVar)
    Exptime = Exptime  * 300/sum(Exptime) # rescale time to 5 min
    timecounter = timecounter + 300 
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
    
    
    ##### after first 5 min exploration, start exploration after break
    while (timecounter <= 4500){ # there are (85 - 10) * 60 seconds in the experiemnt
      ##### start from the decision phase ######
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
        dwelltime= 0 # rndb
        dwellshift = 0 #rndb, use dwellshift to see whether the choice is changed
        CTime = 0
        print(Subject)
        print(ThisRound)
        print(ThisPhase) #### hhhhhh
        print("after break")
        print(para_name)
        while (Finish <= 4 & timecounter < 4500){
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
          
          
          
          while (shift == 0 & timecounter <= 4500) {### i.e., when the attention does not shift
            ### evidence integration
            CTime = CTime + modeltimestamp
            dwelltime = dwelltime + modeltimestamp
            timecounter = timecounter + modeltimestamp
            ### check for time first
            if (timecounter >= 4500){
              print(paste0("timeout in the middle of choice:",timecounter))
              break}
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
                  dwellshift = 1 # rndb change choice
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
                dwellshift = 1 # rndb change from choice 0 to choice 1
                Item_EI$thres[Item_EI$status == 0] = thres_item_final
              }
            }
            
            if (shift == 1){
              # once the attention shift, we mark the last item
              Item_EI$recovery[Item_EI$ID == attention] = 0
              Item_EI$timevar[Item_EI$ID == attention] = 0
              
              ### attention shift, record the dwell time
              onerecord = data.frame(Subject=Subject,Round=ThisRound,Phase=ThisPhase,
                                     choice=Finish-dwellshift,
                                     PicID=attention,
                                     dwelltime=dwelltime,afterbreak=1
              )
              dwell_table = rbind(dwell_table,onerecord)
              dwelltime = 0
              dwellshift = 0 # set dwellshift to 0 again
              attention = 0
            }
          }
        }
        
        ##### deliver the feedback
        Schema_res = Item_EI$Schema[Item_EI$ID %in% Outputs_cho[ThisPhase,7:10]]
        if (type %in% c("Hc","H","LH")){#high risk after break
          if (length(unique(Schema_res)) == 1){
            Outputs_cho$AC[ThisPhase] = 1
            Outputs_cho$performance[ThisPhase] = sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,7:10]]) * 3
          }else if(length(unique(Schema_res)) == 2 & sum(Schema_res == unique(Schema_res)[1]) != 2){
            # 3/4
            Outputs_cho$AC[ThisPhase] = 0.5
            Outputs_cho$performance[ThisPhase] = sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,7:10]])
          }else{
            Outputs_cho$AC[ThisPhase] = 0
            Outputs_cho$performance[ThisPhase] = sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,7:10]])
          }
        }else{
          if (length(unique(Schema_res)) == 1){
            Outputs_cho$AC[ThisPhase] = 1
            Outputs_cho$performance[ThisPhase] = sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,7:10]]) * 3
          }else if(length(unique(Schema_res)) == 2 & sum(Schema_res == unique(Schema_res)[1]) != 2){
            Outputs_cho$AC[ThisPhase] = 0.5
            right_schema =  as.data.frame(sort(table(Schema_res),decreasing=TRUE))[1,1] # most frequent schema chosen
            Outputs_cho$performance[ThisPhase] = sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,7:10]]) + schemainfo$payoff[schemainfo$schemaID==right_schema]*3
          }else{
            Outputs_cho$AC[ThisPhase] = 0
            Outputs_cho$performance[ThisPhase] = sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,7:10]])
          }
        } ## end of deliver feedback
      
        
        schemainfo$afterbreak = 1 # schemacount test
        schemainfo$round = ThisPhase
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
      } # the end of one decision
      ThisRound = ThisRound + 1
      
      ### check for time
      if (timecounter >= 4500){
        print(paste0("timeout before exploration:",timecounter))
        break}
      ### then the exploration phase turn to be 3 min 
      ## firstly, sample the confidence prior the exploration
      Con_PriorExp = mapply(rnorm, 1, mean = schemainfo$conN, sd = schemainfo$conVar)
      ### we then sample the exploration length
      Exptime = mapply(explengthsample, expN = schemainfo$expN, expVar = schemainfo$expVar)
      Exptime = Exptime  * 180/sum(Exptime) # rescale time to 3 min
      timecounter = timecounter + 180
      schemainfo$afterbreak = 1 # schemacount test
      schemainfo$round = ThisPhase
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
      ### check for time
      if (timecounter >= 4500){
        print(paste0("timeout after exploration:",timecounter))
        break}
    } # the end of after break
    

    # novelty col: 0 - schema from old schema, 1 - schema from new schema
    if(type %in% c("H","L")){
      changeschema <- expschema %>% filter(type=='new') %>% select(schemaID)
      Outputs_cho$novelty = Outputs_cho$Schema %in% changeschema
    } else{
      Outputs_cho$novelty = FALSE
    }
    Outputs_cho$afterbreak = 1 ## after break, marked as 1
    Outputs_cho$breakR = breakR ## add the break round
    
    ### whenever new things are added and code doesn't work, run the following to check
    # print(colnames(ALL.df))
    # print(colnames(Outputs_cho))
    
    ## filter the 0 rows
    Outputs_cho = Outputs_cho %>% filter(Cho_1 != 0)
    rndb = Outputs_cho
    ALL.df <- rbind(ALL.df, Outputs_cho)
    Outputs_learn <- cbind(Outputs_learn, Subject=rep(Subject, nrow(Outputs_learn)))
    Outputs_glearn <- cbind(Outputs_glearn, Subject=rep(Subject, nrow(Outputs_glearn)))
    confidence.df <- rbind(confidence.df, Outputs_learn)
    gconfidence.df <- rbind(gconfidence.df, Outputs_glearn)
    dwell_all_subject <- rbind(dwell_all_subject,dwell_table)
  }
  
  ALL.df <- merge(ALL.df,Param.df[,c("SubjectID",para_name)],
                  by.x= "Subject",by.y = "SubjectID") # how to tag the para on it
  dwmall <- dwell_all_subject %>%
    mutate(choice = replace(choice, choice == 0, 1)) %>%
    group_by(Subject,Round,Phase,choice,afterbreak) %>%
    summarise(dwell_mean=mean(dwelltime)) %>%
    pivot_wider(names_from = choice,
                names_glue = 'dwellmean_{choice}',
                values_from = dwell_mean)
  # include afterbreak in ALL.df to merge with dwmall
  allresult <- merge(ALL.df,dwmall,by=c("Subject",'Round',"Phase","afterbreak")) # with dwell time on it
  allresult <- allresult %>% arrange(Subject,Phase)
  # selection1 modify(add schema value)
  allresult <- allresult %>%
    mutate(RT_1 = Schema_RT + RT_1,
           OB_1 = Schema_OB + OB_1,
           AS_1 = Schema_AS + AS_1)
  allresult$OB_1[allresult$OB_1 > 20] <- 20
  allresult$OB_2[allresult$OB_2 > 20] <- 20
  allresult$OB_3[allresult$OB_3 > 20] <- 20
  allresult$OB_4[allresult$OB_4 > 20] <- 20
  # remove rows with na
  allresult = allresult[complete.cases(allresult), ]
  
  ## save results
  savepath = file.path("data",run_name)
  write_csv(ALL.df,file.path(savepath,'/allresult.csv'))
  write_csv(Param.df, file.path(savepath,'/Paras.csv'))
  write_csv(confidence.df, file.path(savepath,'/confidence.csv'))
  write_csv(gconfidence.df, file.path(savepath, '/gconfidence.csv'))
  write_csv(dwell_all_subject, file.path(savepath,'/dwell_table.csv'))
  write_csv(allresult,file.path(savepath,'/all_dwsch_added.csv'))
  return(allresult)
}




# set seed for random number generator
set.seed(12)

if (!dir.exists(file.path("data","model_noexpfam","test"))){
  dir.create(file.path("data","model_noexpfam","test"),recursive = T)
}  
simupath = file.path("model_noexpfam","test")


###### following codes are used to test whether the function above works
simu<- run_model(run_name = file.path(simupath,'test'),
                  para_name = "modeltimestamp",para_list = c(1),
                  type = "L")


###### Now run simulation
subject_num = 40 # how many people take the experiment

# Low risk with novelty
ALL.df <- run_model(run_name = file.path(simupath,'L'),
                    para_name = "thres_schema",
                    para_list = rep(40,subject_num),
                    type = "L")

# High risk with novelty
ALL.df <- run_model(run_name = file.path(simupath,'H'),
                    para_name = "thres_schema",
                    para_list = rep(40,subject_num),
                    type = "H")


# high to low
ALL.df <- run_model(run_name = file.path(simupath,'HL'),
                    para_name = "thres_schema",
                    para_list = rep(40,subject_num),
                    type = "HL")

# low to high
ALL.df <- run_model(run_name = file.path(simupath,'LH'),
                    para_name = "thres_schema",
                    para_list = rep(40,subject_num),
                    type = "LH")

# Low control
ALL.df <- run_model(run_name = file.path(simupath,'Lc'),
                    para_name = "thres_schema",
                    para_list = rep(40,subject_num),
                    type = "Lc")

# high control
ALL.df <- run_model(run_name = file.path(simupath,'Hc'),
                    para_name = "thres_schema",
                    para_list = rep(40,subject_num),
                    type = "Hc")