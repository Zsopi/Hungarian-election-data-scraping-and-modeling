library(data.table)
library(dplyr)
library(ggplot2)

# setting locale for accented character handling
ifelse(.Platform$OS.type == 'windows', 
       Sys.setlocale(category = 'LC_ALL', locale = 'Hungarian'), 
       Sys.setlocale(category = 'LC_ALL', locale = 'hu_HU.utf8'))

#setting working directory, change it as desired, otherwise uses default working directory
wd<-getwd()
#wd<-'C:/Users/Zsopi/Google Drive/R/Scraping'

#valker_result_df<-data.table(read.csv(paste0(wd,'/valker_results.csv')))
#valkor_results_df<-data.table(read.csv(paste0(wd,'/valkor_results.csv')))

#reading in scraped election data from my Github page
valker_result_df<-data.table(read.csv('https://raw.githubusercontent.com/Zsopi/Hungarian-election-data-scraping-and-modeling/master/valker_results.csv'))
valkor_results_df<-data.table(read.csv('https://raw.githubusercontent.com/Zsopi/Hungarian-election-data-scraping-and-modeling/master/valkor_results.csv'))


#function to calculate parlamentary seats in case of constant number of votes for Fidesz
#result depends on additional number of non-Fidesz) voters, voting participation
#additional participation and votes are distributed as follows:
#there is the same percentage increase in participarion rate in every voting district, regardless of past participation rate
#additional voters are all assumed to be opposition voters,
#voting in the same proportion as the relative strength of opposition parties in a given district
voting_share<-61.26499

get_seats_const_fidesz<-function(voting_share) {
        #generate additional vote number, distribute it evenly accross districts and within districts, parties
        additional_voting_share<-voting_share-61.26499
        tt_hypo<-cbind(valker_result_df, add_votes=round(valker_result_df$AE*additional_voting_share/100))
        setDT(tt_hypo)
        tt_hypo[,`:=`(
                MSZP_egy=MSZP_egy+round(MSZP_egy_pct/(1-FIDESZ_egy_pct)*add_votes),
                JOBBIK_egy=JOBBIK_egy+round(JOBBIK_egy_pct/(1-FIDESZ_egy_pct)*add_votes),
                LMP_egy=LMP_egy+round(LMP_egy_pct/(1-FIDESZ_egy_pct)*add_votes),
                MAS_egy=MAS_egy+round(MAS_egy_pct/(1-FIDESZ_egy_pct)*add_votes),
                MSZP_lst=MSZP_lst+round(MSZP_lst_pct/(1-FIDESZ_lst_pct)*add_votes),
                JOBBIK_lst=JOBBIK_lst+round(JOBBIK_lst_pct/(1-FIDESZ_lst_pct)*add_votes),
                LMP_lst=LMP_lst+round(LMP_lst_pct/(1-FIDESZ_lst_pct)*add_votes),
                MAS_lst=MAS_lst+round(MAS_lst_pct/(1-FIDESZ_lst_pct)*add_votes),
                FE=(FIDESZ_lst+MSZP_lst+JOBBIK_lst+LMP_lst+MAS_lst),
                FIDESZ_lst_pct=FIDESZ_lst/FE,
                MSZP_lst_pct=MSZP_lst/FE,
                JOBBIK_lst_pct=JOBBIK_lst/FE,
                LMP_lst_pct=LMP_lst/FE,
                MAS_lst_pct=MAS_lst/FE
                
        )]
        
        #calculating "fragment" votes (votes that did not result in a mandate or was not necessary for winning)
        #defining varnames and filling in with all the votes initially
        tt_hypo[,paste0(names(tt_hypo[,FIDESZ_lst:MAS_lst]),"_frm"):=.(FIDESZ_egy,MSZP_egy,JOBBIK_egy,LMP_egy,MAS_egy)]
        
        #getting a list of values to be substituted
        reduced_max<-apply(tt_hypo[,FIDESZ_lst_frm:MAS_lst_frm], 1, function(z){z[order(z, decreasing=TRUE)[1]]=z[order(z, decreasing=TRUE)[1]]-z[order(z, decreasing=TRUE)[2]]}) 
        index_max<-apply(tt_hypo[,FIDESZ_lst_frm:MAS_lst_frm], 1, function(z){order(z, decreasing=TRUE)[1]})
        
        #updating max values in a loop
        #how to do this more elegantly with apply?
        for (i in seq(index_max)){tt_hypo[i,colnames(tt_hypo[,FIDESZ_lst_frm:MAS_lst_frm])[index_max[i]]:=reduced_max[i]]}
        
        #calculating total of list votes and total of list votes adjusted with fragment votes
        tt_listvotes<-(tt_hypo[,lapply(.SD,sum),.SDcols=FIDESZ_lst:MAS_lst])
        tt_listvotes_adj<-(tt_hypo[,lapply(.SD,sum),.SDcols=FIDESZ_lst_frm:MAS_lst_frm]+tt_hypo[,lapply(.SD,sum),.SDcols=FIDESZ_lst:MAS_lst])
        
        tt_listpct<-tt_listvotes/sum(tt_listvotes)
        names(tt_listpct)<-names(tt_hypo[,FIDESZ_lst_pct:MAS_lst_pct])
        
        #adding foreign mail votes for Fidesz
        tt_listvotes_adj[1,FIDESZ_lst_frm:=FIDESZ_lst_frm+120000]
        
        tt_listpct_adj<-tt_listvotes_adj/sum(tt_listvotes_adj)
        
        tt_listpct_adj[1,MAS_lst_frm:=0]
        
        tt_listpct_adj<-tt_listpct_adj/sum(tt_listpct_adj)
        
        seats<-round(tt_listpct_adj*93)
        names(seats)<-sapply(names(seats), function(x) gsub('_frm',"",as.character(x)))
        seats<-cbind(tt_listpct,seats)
        
        setDT(seats)
        
        seats<-cbind(data.table("reszvetel"=voting_share),seats)
        
        seats<-cbind(seats,data.table("FIDESZ_egy"=0,"MSZP_egy"=0,"JOBBIK_egy"=0,"LMP_egy"=0,"MAS_egy"=0,"FIDESZ_sum"=0,"MSZP_sum"=0,"JOBBIK_sum"=0,"LMP_sum"=0,"MAS_sum"=0))
        
        winners<-colnames(tt_hypo[,FIDESZ_egy:MAS_egy])[apply(tt_hypo[,FIDESZ_egy:MAS_egy],1,which.max)]
        
        seats[1,FIDESZ_egy:=length(winners[winners=='FIDESZ_egy'])]
        seats[1,MSZP_egy:=length(winners[winners=='MSZP_egy'])]
        seats[1,JOBBIK_egy:=length(winners[winners=='JOBBIK_egy'])]
        seats[1,LMP_egy:=length(winners[winners=='LMP_egy'])]
        
        seats[1,FIDESZ_sum:=FIDESZ_egy+FIDESZ_lst]
        seats[1,MSZP_sum:=MSZP_egy+MSZP_lst]
        seats[1,JOBBIK_sum:=JOBBIK_egy+JOBBIK_lst]
        seats[1,LMP_sum:=LMP_egy+LMP_lst]
        seats[1,MAS_sum:=MAS_egy+MAS_lst]
        
        return(seats)
        #####
}

a<-get_seats_const_fidesz(73.51)

seat_scenarios <- lapply(seq(60,78,by=0.5), get_seats_const_fidesz)

seat_scenarios_dt <- rbindlist(seat_scenarios)


#Calculating strategy that results in largest number of opposition candidates
#same function as above, but assumes that opposition parties step down where it is worth it (assuming they know their conversion rates)

#first set 'conversion rates' (who voters vote for when their favorite candidate is not running)
#these are proportions of the original number of votes

J_to_M<-0.5 #Jobbik conversion rates to MSZP when only MSZP candidate is standing on the opposition side. It means in the counterfactual 40% of the original Jobbik votes are cast for MSZP candidates
J_to_F<-0.3 #Jobbik conversion rates to FIDESZ when no Jobbik candidate is standing
J_to_L<-0   #Jobbik conversion rates to LMP  when only LMP candidate is standing on the opposition side
M_to_J<-0.5 #MSZP conversion rates to Jobbik  when only Jobbik candidate is standing on the opposition side
M_to_L<-0.7 #MSZP conversion rates to LMP  when only LMP candidate is standing on the opposition side
L_to_M<-0.2 #LMP conversion rates to MSZP  when only MSZP candidate is standing on the opposition side
L_to_J<-0   #LMP conversion rates to Jobbik  when only Jobbik candidate is standing on the opposition side


J_to_M<-0
J_to_F<-0
J_to_L<-0
M_to_J<-0
M_to_L<-0
L_to_M<-0
L_to_J<-0

voting_share=61.26499

get_seats_const_fidesz_plus_coord<-function(voting_share=68) {
        #generate additional vote number, distribute it evenly accross districts and within districts, parties
        additional_voting_share<-voting_share-61.26499
        tt_hypo<-cbind(valker_result_df, add_votes=round(valker_result_df$AE*additional_voting_share/100))
        setDT(tt_hypo)
        tt_hypo[,`:=`(
                MSZP_egy=MSZP_egy+round(MSZP_egy_pct/(1-FIDESZ_egy_pct)*add_votes),
                JOBBIK_egy=JOBBIK_egy+round(JOBBIK_egy_pct/(1-FIDESZ_egy_pct)*add_votes),
                LMP_egy=LMP_egy+round(LMP_egy_pct/(1-FIDESZ_egy_pct)*add_votes),
                MAS_egy=MAS_egy+round(MAS_egy_pct/(1-FIDESZ_egy_pct)*add_votes),
                MSZP_lst=MSZP_lst+round(MSZP_lst_pct/(1-FIDESZ_lst_pct)*add_votes),
                JOBBIK_lst=JOBBIK_lst+round(JOBBIK_lst_pct/(1-FIDESZ_lst_pct)*add_votes),
                LMP_lst=LMP_lst+round(LMP_lst_pct/(1-FIDESZ_lst_pct)*add_votes),
                MAS_lst=MAS_lst+round(MAS_lst_pct/(1-FIDESZ_lst_pct)*add_votes),
                FE=(FIDESZ_lst+MSZP_lst+JOBBIK_lst+LMP_lst+MAS_lst),
                FIDESZ_lst_pct=FIDESZ_lst/FE,
                MSZP_lst_pct=MSZP_lst/FE,
                JOBBIK_lst_pct=JOBBIK_lst/FE,
                LMP_lst_pct=LMP_lst/FE,
                MAS_lst_pct=MAS_lst/FE
                
        )]
        
        baseline_winners<-colnames(tt_hypo[,FIDESZ_egy:MAS_egy])[apply(tt_hypo[,FIDESZ_egy:MAS_egy],1,which.max)]
        baseline_winners<-c('FIDESZ','MSZP','JOBBIK','LMP')[apply(tt_hypo[,FIDESZ_egy:LMP_egy], 1, function(z){order(z, decreasing=TRUE)[1]})]
        
        #calculating alternative individual district outcome in case of one opposition candidate stepdowns
        tt_hypo[,`:=`(
                MSZP_egy_alt=MSZP_egy+round(J_to_M*JOBBIK_egy+L_to_M*LMP_egy),
                JOBBIK_egy_alt=JOBBIK_egy+round(M_to_J*MSZP_egy+L_to_J*LMP_egy),
                LMP_egy_alt=LMP_egy+round(M_to_L*MSZP_egy+J_to_L*JOBBIK_egy),
                
                #these are calculated to take into account the potential switch from jobbik to fidesz
                FIDESZ_egy_alt_c=FIDESZ_egy,
                MSZP_egy_alt_c=MSZP_egy+round(J_to_M*JOBBIK_egy+L_to_M*LMP_egy-J_to_F*JOBBIK_egy),
                JOBBIK_egy_alt_c=JOBBIK_egy+round(M_to_J*MSZP_egy+L_to_J*LMP_egy),
                LMP_egy_alt_c=LMP_egy+round(M_to_L*MSZP_egy+J_to_L*JOBBIK_egy-J_to_F*JOBBIK_egy)
        )]
        
        alt_winner<-c('FIDESZ','MSZP','JOBBIK','LMP')[apply(tt_hypo[,FIDESZ_egy_alt_c:LMP_egy_alt_c],1,which.max)]
        alt_second<-c('FIDESZ','MSZP','JOBBIK','LMP')[apply(tt_hypo[,FIDESZ_egy_alt_c:LMP_egy_alt_c], 1, function(z){order(z, decreasing=TRUE)[2]})]
        alt_third<-c('FIDESZ','MSZP','JOBBIK','LMP')[apply(tt_hypo[,FIDESZ_egy_alt_c:LMP_egy_alt_c], 1, function(z){order(z, decreasing=TRUE)[3]})]
        
        #getting index of winners in no stepdown original case 
        index_max<-apply(tt_hypo[,FIDESZ_egy:MSZP_egy], 1, function(z){order(z, decreasing=TRUE)[1]})
        #getting index of winners in case of 1 vs 1 races against Fidesz
        index_max_1v1<-apply(tt_hypo[,FIDESZ_egy_alt_c:LMP_egy_alt_c], 1, function(z){order(z, decreasing=TRUE)[1]})
        
        #updating individual district outcomes in a loop, assuming optimal stand-down strategy from the opposition
        for (i in seq(index_max_1v1)){
                if (index_max_1v1[i]!=1&index_max[i]==1) {
                        tt_hypo[i,colnames(tt_hypo[,MSZP_egy:LMP_egy]):=0]
                        tt_hypo[i,colnames(tt_hypo[,FIDESZ_egy:LMP_egy])[index_max_1v1[i]]:=tt_hypo[i,(colnames(tt_hypo[,MSZP_egy_alt:LMP_egy_alt])[index_max_1v1[i]-1]),with=F]]
                }
        }
        
        
        
        
        #calculating "fragment" votes (votes that did not result in a mandate or was not necessary for winning)
        #defining varnames and filling in with all the votes initially
        tt_hypo[,paste0(names(tt_hypo[,FIDESZ_lst:MAS_lst]),"_frm"):=.(FIDESZ_egy,MSZP_egy,JOBBIK_egy,LMP_egy,MAS_egy)]
        
        #getting a list of values to be substituted
        reduced_max<-apply(tt_hypo[,FIDESZ_lst_frm:MAS_lst_frm], 1, function(z){z[order(z, decreasing=TRUE)[1]]=z[order(z, decreasing=TRUE)[1]]-z[order(z, decreasing=TRUE)[2]]}) 
        index_max<-apply(tt_hypo[,FIDESZ_lst_frm:MAS_lst_frm], 1, function(z){order(z, decreasing=TRUE)[1]})
        
        #updating max values in a loop
        #how to do this more elegantly with apply?
        for (i in seq(index_max)){tt_hypo[i,colnames(tt_hypo[,FIDESZ_lst_frm:MAS_lst_frm])[index_max[i]]:=reduced_max[i]]}
        
        #calculating total of list votes and total of list votes adjusted with fragment votes
        tt_listvotes<-(tt_hypo[,lapply(.SD,sum),.SDcols=FIDESZ_lst:MAS_lst])
        tt_listvotes_adj<-(tt_hypo[,lapply(.SD,sum),.SDcols=FIDESZ_lst_frm:MAS_lst_frm]+tt_hypo[,lapply(.SD,sum),.SDcols=FIDESZ_lst:MAS_lst])
        
        tt_listpct<-tt_listvotes/sum(tt_listvotes)
        names(tt_listpct)<-names(tt_hypo[,FIDESZ_lst_pct:MAS_lst_pct])
        
        #adding foreign mail votes for Fidesz
        tt_listvotes_adj[1,FIDESZ_lst_frm:=FIDESZ_lst_frm+120000]
        
        tt_listpct_adj<-tt_listvotes_adj/sum(tt_listvotes_adj)
        
        tt_listpct_adj[1,MAS_lst_frm:=0]
        
        tt_listpct_adj<-tt_listpct_adj/sum(tt_listpct_adj)
        
        #calculating seats
        seats<-round(tt_listpct_adj*93)
        names(seats)<-sapply(names(seats), function(x) gsub('_frm',"",as.character(x)))
        seats<-cbind(tt_listpct,seats)
        
        setDT(seats)
        
        seats<-cbind(data.table("reszvetel"=voting_share),seats)
        
        seats<-cbind(seats,data.table("FIDESZ_egy"=0,"MSZP_egy"=0,"JOBBIK_egy"=0,"LMP_egy"=0,"MAS_egy"=0,"FIDESZ_sum"=0,"MSZP_sum"=0,"JOBBIK_sum"=0,"LMP_sum"=0,"MAS_sum"=0))
        
        winners<-colnames(tt_hypo[,FIDESZ_egy:MAS_egy])[apply(tt_hypo[,FIDESZ_egy:MAS_egy],1,which.max)]
        
        seats[1,FIDESZ_egy:=length(winners[winners=='FIDESZ_egy'])]
        seats[1,MSZP_egy:=length(winners[winners=='MSZP_egy'])]
        seats[1,JOBBIK_egy:=length(winners[winners=='JOBBIK_egy'])]
        
        seats[1,FIDESZ_sum:=FIDESZ_egy+FIDESZ_lst]
        seats[1,MSZP_sum:=MSZP_egy+MSZP_lst]
        seats[1,JOBBIK_sum:=JOBBIK_egy+JOBBIK_lst]
        seats[1,LMP_sum:=LMP_egy+LMP_lst]
        seats[1,MAS_sum:=MAS_egy+MAS_lst]
        
        return(seats)
        
        margin_over_FIDESZ<-cbind(tt_hypo[,MEGYE:SZEKHELY],baseline_winner=baseline_winners,alt_winner,alt_second,alt_third,tt_hypo[,FIDESZ_egy_alt_c:LMP_egy_alt_c])
        margin_over_FIDESZ[,MSZP_egyedul_mrg:=MSZP_egy_alt_c-FIDESZ_egy_alt_c]
        margin_over_FIDESZ[,JOBBIK_egyedul_mrg:=JOBBIK_egy_alt_c-FIDESZ_egy_alt_c]
        margin_over_FIDESZ[,LMP_egyedul_mrg:=LMP_egy_alt_c-FIDESZ_egy_alt_c]
        
        margin_over_FIDESZ[,OPP_max:=apply (margin_over_FIDESZ[,MSZP_egyedul_mrg:LMP_egyedul_mrg],1,function(z){z[order (z,decreasing=T)[1]]}) ]
        
        margin_over_FIDESZ<- margin_over_FIDESZ[order(OPP_max,decreasing=T),]
        
        margin_over_FIDESZ[,colnames(margin_over_FIDESZ[,FIDESZ_egy_alt_c:LMP_egy_alt_c]):=NULL]
        
        #return(margin_over_FIDESZ[baseline_winner=='FIDESZ'&alt_winner!='FIDESZ'])
        
        out<-list(seats,margin_over_FIDESZ[baseline_winner=='FIDESZ'&alt_winner!='FIDESZ'])
        #return(out)
        
        
        #####
}

#assuming whole opposition coordination, 50% cross votes, 30% of Jobbik voters for Fidesz if no Jobbik candidate
J_to_M<-0.5
J_to_F<-0.3
J_to_L<-0.5
M_to_J<-0.5
M_to_L<-0.5
L_to_M<-0.5
L_to_J<-0.5

seat_scenarios_coord <- lapply(seq(60,78,by=0.5), get_seats_const_fidesz_plus_coord)
seat_scenarios_coord_dt <- rbindlist(seat_scenarios_coord)



#assuming coordination only on the left
J_to_M<-0
J_to_F<-0
J_to_L<-0
M_to_J<-0
M_to_L<-0.8
L_to_M<-0.8
L_to_J<-0

seat_scenarios_coord_left <- lapply(seq(60,78,by=0.5), get_seats_const_fidesz_plus_coord)
seat_scenarios_coord_left_dt <- rbindlist(seat_scenarios_coord_left)


View(margin_over_FIDESZ[baseline_winner=='FIDESZ'&alt_winner!='FIDESZ'])

J_to_M<-0
J_to_F<-0
J_to_L<-0
M_to_J<-0
M_to_L<-0.8
L_to_M<-0.8
L_to_J<-0


J_to_M<-1
J_to_F<-0
J_to_L<-1
M_to_J<-0
M_to_L<-1
L_to_M<-1
L_to_J<-0


out<-get_seats_const_fidesz_plus_coord()
View(out[[1]])
View(out[[2]])

##########SPONTANEOUS COORDINATION

get_seats_const_fidesz_plus_spont_coord<-function(voting_share=68,crossvote=0.3) {
  
  J_to_M<-crossvote
  J_to_F<-0
  J_to_L<-crossvote
  M_to_J<-crossvote
  M_to_L<-crossvote
  L_to_M<-crossvote
  L_to_J<-crossvote
  
  #generate additional vote number, distribute it evenly accross districts and within districts, parties
  additional_voting_share<-voting_share-61.26499
  tt_hypo<-cbind(valker_result_df, add_votes=round(valker_result_df$AE*additional_voting_share/100))
  setDT(tt_hypo)
  tt_hypo[,`:=`(
    MSZP_egy=MSZP_egy+round(MSZP_egy_pct/(1-FIDESZ_egy_pct)*add_votes),
    JOBBIK_egy=JOBBIK_egy+round(JOBBIK_egy_pct/(1-FIDESZ_egy_pct)*add_votes),
    LMP_egy=LMP_egy+round(LMP_egy_pct/(1-FIDESZ_egy_pct)*add_votes),
    MAS_egy=MAS_egy+round(MAS_egy_pct/(1-FIDESZ_egy_pct)*add_votes),
    MSZP_lst=MSZP_lst+round(MSZP_lst_pct/(1-FIDESZ_lst_pct)*add_votes),
    JOBBIK_lst=JOBBIK_lst+round(JOBBIK_lst_pct/(1-FIDESZ_lst_pct)*add_votes),
    LMP_lst=LMP_lst+round(LMP_lst_pct/(1-FIDESZ_lst_pct)*add_votes),
    MAS_lst=MAS_lst+round(MAS_lst_pct/(1-FIDESZ_lst_pct)*add_votes),
    FE=(FIDESZ_lst+MSZP_lst+JOBBIK_lst+LMP_lst+MAS_lst),
    FIDESZ_lst_pct=FIDESZ_lst/FE,
    MSZP_lst_pct=MSZP_lst/FE,
    JOBBIK_lst_pct=JOBBIK_lst/FE,
    LMP_lst_pct=LMP_lst/FE,
    MAS_lst_pct=MAS_lst/FE
    
  )]
  
  baseline_winners<-colnames(tt_hypo[,FIDESZ_egy:MAS_egy])[apply(tt_hypo[,FIDESZ_egy:MAS_egy],1,which.max)]
  baseline_winners<-c('FIDESZ','MSZP','JOBBIK','LMP')[apply(tt_hypo[,FIDESZ_egy:LMP_egy], 1, function(z){order(z, decreasing=TRUE)[1]})]
  
  #calculating alternative individual district outcome in case of one opposition candidate stepdowns
  tt_hypo[,`:=`(
    MSZP_egy_alt=MSZP_egy+round(J_to_M*JOBBIK_egy+L_to_M*LMP_egy),
    JOBBIK_egy_alt=JOBBIK_egy+round(M_to_J*MSZP_egy+L_to_J*LMP_egy),
    LMP_egy_alt=LMP_egy+round(M_to_L*MSZP_egy+J_to_L*JOBBIK_egy),
    
    #these are calculated to take into account the potential switch from jobbik to fidesz
    FIDESZ_egy_alt_c=FIDESZ_egy,
    MSZP_egy_alt_c=MSZP_egy+round(J_to_M*JOBBIK_egy+L_to_M*LMP_egy-J_to_F*JOBBIK_egy),
    JOBBIK_egy_alt_c=JOBBIK_egy+round(M_to_J*MSZP_egy+L_to_J*LMP_egy),
    LMP_egy_alt_c=LMP_egy+round(M_to_L*MSZP_egy+J_to_L*JOBBIK_egy-J_to_F*JOBBIK_egy)
  )]
  
  alt_winner<-c('FIDESZ','MSZP','JOBBIK','LMP')[apply(tt_hypo[,FIDESZ_egy_alt_c:LMP_egy_alt_c],1,which.max)]
  alt_second<-c('FIDESZ','MSZP','JOBBIK','LMP')[apply(tt_hypo[,FIDESZ_egy_alt_c:LMP_egy_alt_c], 1, function(z){order(z, decreasing=TRUE)[2]})]
  alt_third<-c('FIDESZ','MSZP','JOBBIK','LMP')[apply(tt_hypo[,FIDESZ_egy_alt_c:LMP_egy_alt_c], 1, function(z){order(z, decreasing=TRUE)[3]})]
  
  #getting index of winners in no stepdown original case 
  index_max<-apply(tt_hypo[,FIDESZ_egy:MSZP_egy], 1, function(z){order(z, decreasing=TRUE)[1]})
  #getting index of winners in case of 1 vs 1 races against Fidesz
  index_max_1v1<-apply(tt_hypo[,FIDESZ_egy_alt_c:LMP_egy_alt_c], 1, function(z){order(z, decreasing=TRUE)[1]})
  
  #updating individual district outcomes in a loop, assuming optimal stand-down strategy from the opposition
  for (i in seq(index_max_1v1)){
    if (index_max_1v1[i]!=1&index_max[i]==1) {
      tt_hypo[i,lapply(.SD,function (x) x*(1-crossvote)),.SDcols=FIDESZ_egy:LMP_egy]
      tt_hypo[i,colnames(tt_hypo[,FIDESZ_egy:LMP_egy])[index_max_1v1[i]]:=tt_hypo[i,(colnames(tt_hypo[,MSZP_egy_alt:LMP_egy_alt])[index_max_1v1[i]-1]),with=F]]
    }
  }
  
  
  #calculating "fragment" votes (votes that did not result in a mandate or was not necessary for winning)
  #defining varnames and filling in with all the votes initially
  tt_hypo[,paste0(names(tt_hypo[,FIDESZ_lst:MAS_lst]),"_frm"):=.(FIDESZ_egy,MSZP_egy,JOBBIK_egy,LMP_egy,MAS_egy)]
  
  #getting a list of values to be substituted
  reduced_max<-apply(tt_hypo[,FIDESZ_lst_frm:MAS_lst_frm], 1, function(z){z[order(z, decreasing=TRUE)[1]]=z[order(z, decreasing=TRUE)[1]]-z[order(z, decreasing=TRUE)[2]]}) 
  index_max<-apply(tt_hypo[,FIDESZ_lst_frm:MAS_lst_frm], 1, function(z){order(z, decreasing=TRUE)[1]})
  
  #updating max values in a loop
  #how to do this more elegantly with apply?
  for (i in seq(index_max)){tt_hypo[i,colnames(tt_hypo[,FIDESZ_lst_frm:MAS_lst_frm])[index_max[i]]:=reduced_max[i]]}
  
  #calculating total of list votes and total of list votes adjusted with fragment votes
  tt_listvotes<-(tt_hypo[,lapply(.SD,sum),.SDcols=FIDESZ_lst:MAS_lst])
  tt_listvotes_adj<-(tt_hypo[,lapply(.SD,sum),.SDcols=FIDESZ_lst_frm:MAS_lst_frm]+tt_hypo[,lapply(.SD,sum),.SDcols=FIDESZ_lst:MAS_lst])
  
  tt_listpct<-tt_listvotes/sum(tt_listvotes)
  names(tt_listpct)<-names(tt_hypo[,FIDESZ_lst_pct:MAS_lst_pct])
  
  #adding foreign mail votes for Fidesz
  tt_listvotes_adj[1,FIDESZ_lst_frm:=FIDESZ_lst_frm+120000]
  
  tt_listpct_adj<-tt_listvotes_adj/sum(tt_listvotes_adj)
  
  tt_listpct_adj[1,MAS_lst_frm:=0]
  
  tt_listpct_adj<-tt_listpct_adj/sum(tt_listpct_adj)
  
  #calculating seats
  seats<-round(tt_listpct_adj*93)
  names(seats)<-sapply(names(seats), function(x) gsub('_frm',"",as.character(x)))
  seats<-cbind(tt_listpct,seats)
  
  setDT(seats)
  
  seats<-cbind(data.table("reszvetel"=voting_share),seats)
  
  seats<-cbind(seats,data.table("FIDESZ_egy"=0,"MSZP_egy"=0,"JOBBIK_egy"=0,"LMP_egy"=0,"MAS_egy"=0,"FIDESZ_sum"=0,"MSZP_sum"=0,"JOBBIK_sum"=0,"LMP_sum"=0,"MAS_sum"=0))
  
  winners<-colnames(tt_hypo[,FIDESZ_egy:MAS_egy])[apply(tt_hypo[,FIDESZ_egy:MAS_egy],1,which.max)]
  
  seats[1,FIDESZ_egy:=length(winners[winners=='FIDESZ_egy'])]
  seats[1,MSZP_egy:=length(winners[winners=='MSZP_egy'])]
  seats[1,JOBBIK_egy:=length(winners[winners=='JOBBIK_egy'])]
  
  seats[1,FIDESZ_sum:=FIDESZ_egy+FIDESZ_lst]
  seats[1,MSZP_sum:=MSZP_egy+MSZP_lst]
  seats[1,JOBBIK_sum:=JOBBIK_egy+JOBBIK_lst]
  seats[1,LMP_sum:=LMP_egy+LMP_lst]
  seats[1,MAS_sum:=MAS_egy+MAS_lst]
  
  return(seats)
  
  margin_over_FIDESZ<-cbind(tt_hypo[,MEGYE:SZEKHELY],baseline_winner=baseline_winners,alt_winner,alt_second,alt_third,tt_hypo[,FIDESZ_egy_alt_c:LMP_egy_alt_c])
  margin_over_FIDESZ[,MSZP_egyedul_mrg:=MSZP_egy_alt_c-FIDESZ_egy_alt_c]
  margin_over_FIDESZ[,JOBBIK_egyedul_mrg:=JOBBIK_egy_alt_c-FIDESZ_egy_alt_c]
  margin_over_FIDESZ[,LMP_egyedul_mrg:=LMP_egy_alt_c-FIDESZ_egy_alt_c]
  
  margin_over_FIDESZ[,OPP_max:=apply (margin_over_FIDESZ[,MSZP_egyedul_mrg:LMP_egyedul_mrg],1,function(z){z[order (z,decreasing=T)[1]]}) ]
  
  margin_over_FIDESZ<- margin_over_FIDESZ[order(OPP_max,decreasing=T),]
  
  margin_over_FIDESZ[,colnames(margin_over_FIDESZ[,FIDESZ_egy_alt_c:LMP_egy_alt_c]):=NULL]
  
  #return(margin_over_FIDESZ[baseline_winner=='FIDESZ'&alt_winner!='FIDESZ'])
  
  out<-list(seats,margin_over_FIDESZ[baseline_winner=='FIDESZ'&alt_winner!='FIDESZ'])
  #return(out)
  
  
  #####
}

seat_scenarios_coord_spont <- lapply(seq(60,78,by=0.5), get_seats_const_fidesz_plus_spont_coord)
seat_scenarios_coord_spont_dt <- rbindlist(seat_scenarios_coord_spont)

seat_scenarios_coord_spont_10 <- lapply(seq(60,78,by=0.5), get_seats_const_fidesz_plus_spont_coord, crossvote=0.1)
seat_scenarios_coord_spont_10_dt <- rbindlist(seat_scenarios_coord_spont_10)

seat_scenarios_coord_spont_20 <- lapply(seq(60,78,by=0.5), get_seats_const_fidesz_plus_spont_coord, crossvote=0.2)
seat_scenarios_coord_spont_20_dt <- rbindlist(seat_scenarios_coord_spont_20)

seat_scenarios_coord_spont_30 <- lapply(seq(60,78,by=0.5), get_seats_const_fidesz_plus_spont_coord, crossvote=0.3)
seat_scenarios_coord_spont_30_dt <- rbindlist(seat_scenarios_coord_spont_30)




##########UPDATE FOR 2018
valker_result_df_18<-valker_result_df<-data.table(read.csv('https://raw.githubusercontent.com/Zsopi/Hungarian-election-data-scraping-and-modeling/master/valker_results.csv'))

valker_result_df_18[,FIDESZ_dstr:=FIDESZ_lst/sum(FIDESZ_lst)]
valker_result_df_18[,MSZP_dstr:=MSZP_lst/sum(MSZP_lst)]
valker_result_df_18[,JOBBIK_dstr:=JOBBIK_lst/sum(JOBBIK_lst)]
valker_result_df_18[,LMP_dstr:=LMP_lst/sum(LMP_lst)]

valker_result_df_18[,EGY_dstr:=LMP_lst/sum(LMP_lst)]
valker_result_df_18[,MOM_dstr:=LMP_lst/sum(LMP_lst)]
valker_result_df_18[,MAS_dstr:=mean(MAS_lst/sum(MAS_lst),LMP_lst/sum(LMP_lst))]

total_2018_votes_dom<-7897198
total_2018_votes_for<-378521

c('FIDESZ_vts')


valker_result_df_18[,FIDESZ_egy_18:=FIDESZ_dstr*FIDESZ_vts]

read_html('http://www.valasztas.hu/dyn/pv14/szavossz/hu/orszlist.html')


############################CHARTS
#Fidesz seats
ggplot(data = seat_scenarios_dt[,.(reszvetel,FIDESZ_sum)], aes(reszvetel,FIDESZ_sum))+geom_step(size=1)+
geom_hline(yintercept=c(133,100),color='red', size=1)+
        ggtitle('Részvétel és FIDESZ képviselők \nfeltételezett száma')+
        labs(x='Részvétel, %',y='FIDESZ képviselők száma')+
        annotate("text", x = c(64,75), y = c(103,130), label = c("parlamenti 50%+", "parlamenti 2/3+"),color='red',size=7)+
        theme(plot.title = element_text( color="#666666", face="bold", size=22, hjust=0.5)) +
        theme(axis.title = element_text( color="#666666", face="bold", size=16))+
        theme(axis.text = element_text( color="#666666", face="bold", size=12)) 

#Fidesz seats with and without coordination
ggplot(data = seat_scenarios_dt[,.(reszvetel,FIDESZ_sum)], aes(reszvetel,FIDESZ_sum))+geom_step(size=1)+
  geom_step(data=seat_scenarios_coord_dt[,.(reszvetel,FIDESZ_sum)],size=1,color="blue")+
  geom_step(data=seat_scenarios_coord_left_dt[,.(reszvetel,FIDESZ_sum)],size=1,color="green")+
  geom_hline(yintercept=c(133,100),color='red', size=1)+
  ggtitle('Részvétel és FIDESZ képviselők \nfeltételezett száma')+
  labs(x='Részvétel, %',y='FIDESZ képviselők száma')+
  annotate("text", x = c(63,75), y = c(95,130), label = c("parlamenti 50%+", "parlamenti 2/3+"),color='red',size=7)+
  annotate("label", x = 65, y = 70, label = "Optimális ellenzéki koordináció,\n 50%-os ellenzéki átszavazás,\n Jobbik szavazók 30%-a a Fideszre szavaz,\n ha nincs saját jelölt" ,color='blue',size=4)+
  annotate("label", x = 74, y = 122, label = "Nincs ellenzéki koordináció, visszalépés" ,color='black',size=4)+
  annotate("label", x = 76.5, y = 77, label = "MSZP-stb-LMP\n koordináció\n 80%-os\nátszavazás" ,color='green',size=4)+
  theme(plot.title = element_text( color="#666666", face="bold", size=22, hjust=0.5)) +
  theme(axis.title = element_text( color="#666666", face="bold", size=16))+
  theme(axis.text = element_text( color="#666666", face="bold", size=12)) 

#Fidesz seats with and without coordination (incl. spontaneous)
ggplot(data = seat_scenarios_dt[,.(reszvetel,FIDESZ_sum)], aes(reszvetel,FIDESZ_sum))+geom_step(size=1)+
  geom_step(data=seat_scenarios_coord_dt[,.(reszvetel,FIDESZ_sum)],size=1,color="blue")+
  geom_step(data=seat_scenarios_coord_left_dt[,.(reszvetel,FIDESZ_sum)],size=1,color="green")+
  geom_step(data=seat_scenarios_coord_spont_dt[,.(reszvetel,FIDESZ_sum)],size=1,color="red")+
  geom_hline(yintercept=c(133,100),color='red', size=1)+
  ggtitle('Részvétel és FIDESZ képviselők \nfeltételezett száma')+
  labs(x='Részvétel, %',y='FIDESZ képviselők száma')+
  annotate("text", x = c(63,75), y = c(95,130), label = c("parlamenti 50%+", "parlamenti 2/3+"),color='red',size=7)+
  annotate("label", x = 65, y = 70, label = "Optimális ellenzéki koordináció,\n 50%-os ellenzéki átszavazás,\n Jobbik szavazók 30%-a a Fideszre szavaz,\n ha nincs saját jelölt" ,color='blue',size=4)+
  annotate("label", x = 74, y = 122, label = "Nincs ellenzéki koordináció, visszalépés" ,color='black',size=4)+
  annotate("label", x = 76.5, y = 77, label = "MSZP-stb-LMP\n koordináció\n 80%-os\nátszavazás" ,color='green',size=4)+
  annotate("label", x = 68, y = 55, label = "Spontán 30%-os átszavazás az esélyes ellenzéki jelöltre, visszalépés nélkül" ,color='red',size=4)+
  annotate("segment", x = 71, xend = 74, y = 58, yend = 80,colour = "red")+
  theme(plot.title = element_text( color="#666666", face="bold", size=22, hjust=0.5)) +
  theme(axis.title = element_text( color="#666666", face="bold", size=16))+
  theme(axis.text = element_text( color="#666666", face="bold", size=12)) 

#Fidesz seats with aspontaneous coordination
ggplot(data=seat_scenarios_coord_spont_10_dt[,.(reszvetel,FIDESZ_sum)],aes(reszvetel,FIDESZ_sum))+
  geom_step(data=seat_scenarios_coord_spont_10_dt[,.(reszvetel,FIDESZ_sum)],size=1,color='#92C5DE')+
  geom_step(data=seat_scenarios_coord_spont_20_dt[,.(reszvetel,FIDESZ_sum)],size=1,color='#F4A582')+
  geom_step(data=seat_scenarios_coord_spont_30_dt[,.(reszvetel,FIDESZ_sum)],size=1,color='#B2182B')+
  geom_hline(yintercept=c(133,100),color='red', size=1)+
  ggtitle('Részvétel és FIDESZ képviselők \nfeltételezett száma')+
  labs(x='Részvétel, %',y='FIDESZ képviselők száma')+
  annotate("text", x = c(63,75), y = c(95,130), label = c("parlamenti 50%+", "parlamenti 2/3+"),color='red',size=7)+
  annotate("label", x = 75, y = 110, label = "Spontán 10%-os átszavazás" ,color='#92C5DE',size=4)+
  annotate("label", x = 75, y = 77, label = "Spontán 20%-os átszavazás" ,color='#F4A582',size=4)+
  annotate("label", x = 75, y = 55, label = "Spontán 30%-os átszavazás" ,color='#B2182B',size=4)+
  #annotate("segment", x = 71, xend = 74, y = 58, yend = 80,colour = "black")+
  theme(plot.title = element_text( color="#666666", face="bold", size=22, hjust=0.5)) +
  theme(axis.title = element_text( color="#666666", face="bold", size=16))+
  theme(axis.text = element_text( color="#666666", face="bold", size=12)) 




#Fidesz ratio
ggplot(data = seat_scenarios_dt[,.(reszvetel,FIDESZ_lst_pct)], aes(reszvetel,FIDESZ_lst_pct*100))+geom_line(size=1)+
        #geom_hline(yintercept=c(133,100),color='red', size=1)+
        ggtitle('Részvétel és FIDESZ \nfeltételezett hazai szavazataránya')+
        labs(x='Részvétel, %',y='FIDESZ részesedése a haza lists szavazatokból')+
        #annotate("text", x = c(62,75), y = c(103,130), label = c("50%+", "2/3+"),color='red',size=7)+
        theme(plot.title = element_text( color="#666666", face="bold", size=22, hjust=0.5)) +
        theme(axis.title = element_text( color="#666666", face="bold", size=16))+
        theme(axis.text = element_text( color="#666666", face="bold", size=12)) 

#Fidesz votes vs attendance
ggplot(data = valkor_results_df[,.(FE_pct,FIDESZ_lst_pct)], aes(FE_pct*100,FIDESZ_lst_pct*100))+geom_point(size=0.5)+
        geom_smooth(data=valkor_results_df[FE_pct>0.3&FE_pct<0.8])+
        ggtitle('Részvétel és a FIDESZ \n szavazataránya választókörönként')+
        labs(x='Részvétel, %',y='FIDESZ  listás szavazatarány %')+
        theme(plot.title = element_text( color="#666666", face="bold", size=22, hjust=0.5)) +
        theme(axis.title = element_text( color="#666666", face="bold", size=16))+
        theme(axis.text = element_text( color="#666666", face="bold", size=12)) 

#MSZP votes vs attendance
ggplot(data = valkor_results_df[,.(FE_pct,MSZP_lst_pct)], aes(FE_pct*100,MSZP_lst_pct*100))+geom_point(size=0.5)+
        geom_smooth(data=valkor_results_df[FE_pct>0.3&FE_pct<0.8])+
        ggtitle('Részvétel és az MSZP\n szavazataránya választókörönként')+
        labs(x='Részvétel, %',y='MSZP listás szavazatarány %')+
        theme(plot.title = element_text( color="#666666", face="bold", size=22, hjust=0.5)) +
        theme(axis.title = element_text( color="#666666", face="bold", size=16))+
        theme(axis.text = element_text( color="#666666", face="bold", size=12)) 

#Jobbik votes vs attendance
ggplot(data = valkor_results_df[,.(FE_pct,JOBBIK_lst_pct)], aes(FE_pct*100,JOBBIK_lst_pct*100))+geom_point(size=0.5)+
        geom_smooth(data=valkor_results_df[FE_pct>0.3&FE_pct<0.8])+
        ggtitle('Részvétel és a Jobbik\n szavazataránya választókörönként')+
        labs(x='Részvétel, %',y='Jobbik listás szavazatarány %')+
        theme(plot.title = element_text( color="#666666", face="bold", size=22, hjust=0.5)) +
        theme(axis.title = element_text( color="#666666", face="bold", size=16))+
        theme(axis.text = element_text( color="#666666", face="bold", size=12)) 

#LMP votes vs attendance
ggplot(data = valkor_results_df[,.(FE_pct,LMP_lst_pct)], aes(FE_pct*100,LMP_lst_pct*100))+geom_point(size=0.5)+
        geom_smooth(data=valkor_results_df[FE_pct>0.3&FE_pct<0.8])+
        ggtitle('Részvétel és az LMP\n szavazataránya választókörönként')+
        labs(x='Részvétel, %',y='LMP listás szavazatarány %')+
        theme(plot.title = element_text( color="#666666", face="bold", size=22, hjust=0.5)) +
        theme(axis.title = element_text( color="#666666", face="bold", size=16))+
        theme(axis.text = element_text( color="#666666", face="bold", size=12)) 



#MSZP votes vs attendance, BP in red
ggplot(data = valkor_results_df[,.(FE_pct,MSZP_lst_pct,MEGYE)], aes(FE_pct*100,MSZP_lst_pct*100))+
        geom_point(data = valkor_results_df[MEGYE=='BUDAPEST',.(FE_pct,MSZP_lst_pct)], color='red',size=0.5)+
        geom_point(data = valkor_results_df[MEGYE!='BUDAPEST',.(FE_pct,MSZP_lst_pct)], color='black',size=0.5)+
        geom_smooth(data=valkor_results_df[FE_pct>0.3&FE_pct<0.8&MEGYE=='BUDAPEST'], color='red')+
        geom_smooth(data=valkor_results_df[FE_pct>0.3&FE_pct<0.8&MEGYE!='BUDAPEST'], color='black')+
        ggtitle('Részvétel és az MSZP\n szavazataránya választókörönként')+
        labs(x='Részvétel, %',y='MSZP listás szavazatarány %')+
        theme(plot.title = element_text( color="#666666", face="bold", size=22, hjust=0.5)) +
        theme(axis.title = element_text( color="#666666", face="bold", size=16))+
        theme(axis.text = element_text( color="#666666", face="bold", size=12)) 


#Fidesz votes vs attendance, BP in red
ggplot(data = valkor_results_df[,.(FE_pct,FIDESZ_lst_pct,MEGYE)], aes(FE_pct*100,FIDESZ_lst_pct*100))+
        geom_point(data = valkor_results_df[MEGYE=='BUDAPEST',.(FE_pct,FIDESZ_lst_pct)], color='red',size=0.5)+
        geom_point(data = valkor_results_df[MEGYE!='BUDAPEST',.(FE_pct,FIDESZ_lst_pct)], color='black',size=0.5)+
        geom_smooth(data=valkor_results_df[FE_pct>0.3&FE_pct<0.8&MEGYE=='BUDAPEST'], color='red')+
        geom_smooth(data=valkor_results_df[FE_pct>0.3&FE_pct<0.8&MEGYE!='BUDAPEST'], color='black')+
        ggtitle('Részvétel és az Fidesz\n szavazataránya választókörönként')+
        labs(x='Részvétel, %',y='Fidesz listás szavazatarány %')+
        theme(plot.title = element_text( color="#666666", face="bold", size=22, hjust=0.5)) +
        theme(axis.title = element_text( color="#666666", face="bold", size=16))+
        theme(axis.text = element_text( color="#666666", face="bold", size=12))


#Jobbik votes vs attendance, BP in red
ggplot(data = valkor_results_df[,.(FE_pct,JOBBIK_lst_pct,MEGYE)], aes(FE_pct*100,JOBBIK_lst_pct*100))+
        geom_point(data = valkor_results_df[MEGYE=='BUDAPEST',.(FE_pct,JOBBIK_lst_pct)], color='red',size=0.5)+
        geom_point(data = valkor_results_df[MEGYE!='BUDAPEST',.(FE_pct,JOBBIK_lst_pct)], color='black',size=0.5)+
        geom_smooth(data=valkor_results_df[FE_pct>0.3&FE_pct<0.8&MEGYE=='BUDAPEST'], color='red')+
        geom_smooth(data=valkor_results_df[FE_pct>0.3&FE_pct<0.8&MEGYE!='BUDAPEST'], color='black')+
        ggtitle('Részvétel és az Jobbik\n szavazataránya választókörönként')+
        labs(x='Részvétel, %',y='Jobbik listás szavazatarány %')+
        theme(plot.title = element_text( color="#666666", face="bold", size=22, hjust=0.5)) +
        theme(axis.title = element_text( color="#666666", face="bold", size=16))+
        theme(axis.text = element_text( color="#666666", face="bold", size=12))


#LMP votes vs attendance, BP in red
ggplot(data = valkor_results_df[,.(FE_pct,LMP_lst_pct,MEGYE)], aes(FE_pct*100,LMP_lst_pct*100))+
        geom_point(data = valkor_results_df[MEGYE=='BUDAPEST',.(FE_pct,LMP_lst_pct)], color='red',size=0.5)+
        geom_point(data = valkor_results_df[MEGYE!='BUDAPEST',.(FE_pct,LMP_lst_pct)], color='black',size=0.5)+
        geom_smooth(data=valkor_results_df[FE_pct>0.3&FE_pct<0.8&MEGYE=='BUDAPEST'], color='red')+
        geom_smooth(data=valkor_results_df[FE_pct>0.3&FE_pct<0.8&MEGYE!='BUDAPEST'], color='black')+
        ggtitle('Részvétel és az LMP\n szavazataránya választókörönként')+
        labs(x='Részvétel, %',y='LMP listás szavazatarány %')+
        theme(plot.title = element_text( color="#666666", face="bold", size=22, hjust=0.5)) +
        theme(axis.title = element_text( color="#666666", face="bold", size=16))+
        theme(axis.text = element_text( color="#666666", face="bold", size=12))




ggplot(data = valkor_results_df[,.(FE_pct,FIDESZ_lst_pct)], aes(FE_pct*100,FIDESZ_lst_pct*100))+geom_point(size=1)+
        geom_smooth()+geom_point(data = valkor_results_df[MEGYE=='BUDAPEST',.(FE_pct,FIDESZ_lst_pct)], color='red')

ggplot(data = valkor_results_df[,.(FE_pct,MSZP_lst_pct)], aes(FE_pct*100,MSZP_lst_pct*100))+geom_point(size=1)+
        geom_smooth()+geom_point(data = valkor_results_df[TELEPUL=='Győr'|TELEPUL=='Miskolc'|TELEPUL=='Szeged'|TELEPUL=='Debrecen'|TELEPUL=='Kecskemét',.(FE_pct,MSZP_lst_pct)], color='red')

ggplot(data = valker_result_df[,.(JE_pct,FIDESZ_lst_pct)], aes(JE_pct*100,FIDESZ_lst_pct*100))+geom_point(size=1)+
        geom_smooth()+
        #geom_hline(yintercept=c(133,100),color='red', size=1)+
        ggtitle('Részvétel és a FIDESZ szavazataránya')+
        labs(x='Részvétel, %',y='FIDESZ részesedése a haza lists szavazatokból')+
        #annotate("text", x = c(62,75), y = c(103,130), label = c("50%+", "2/3+"),color='red',size=7)+
        theme(plot.title = element_text( color="#666666", face="bold", size=22, hjust=0.5)) +
        theme(axis.title = element_text( color="#666666", face="bold", size=16))+
        theme(axis.text = element_text( color="#666666", face="bold", size=12)) 

ggplot(data = valker_result_df[,.(JE_pct,MSZP_lst_pct)], aes(JE_pct*100,MSZP_lst_pct*100))+geom_point(size=1)+
        geom_smooth()+
        #geom_hline(yintercept=c(133,100),color='red', size=1)+
        ggtitle('Részvétel és az MSZP szavazataránya')+
        labs(x='Részvétel, %',y='MSZP részesedése a haza lists szavazatokból')+
        #annotate("text", x = c(62,75), y = c(103,130), label = c("50%+", "2/3+"),color='red',size=7)+
        theme(plot.title = element_text( color="#666666", face="bold", size=22, hjust=0.5)) +
        theme(axis.title = element_text( color="#666666", face="bold", size=16))+
        theme(axis.text = element_text( color="#666666", face="bold", size=12)) 

ggplot(data = valker_result_df[,.(JE_pct,JOBBIK_lst_pct)], aes(JE_pct*100,JOBBIK_lst_pct*100))+geom_point(size=1)+
        geom_smooth()+
        #geom_hline(yintercept=c(133,100),color='red', size=1)+
        ggtitle('Részvétel és a Jobbik szavazataránya')+
        labs(x='Részvétel, %',y='Jobbik részesedése a haza lists szavazatokból')+
        #annotate("text", x = c(62,75), y = c(103,130), label = c("50%+", "2/3+"),color='red',size=7)+
        theme(plot.title = element_text( color="#666666", face="bold", size=22, hjust=0.5)) +
        theme(axis.title = element_text( color="#666666", face="bold", size=16))+
        theme(axis.text = element_text( color="#666666", face="bold", size=12)) 

ggplot(data = valker_result_df[,.(JE_pct,LMP_lst_pct)], aes(JE_pct*100,LMP_lst_pct*100))+geom_point(size=1)+
        geom_smooth()+
        #geom_hline(yintercept=c(133,100),color='red', size=1)+
        ggtitle('Részvétel és az LMP szavazataránya')+
        labs(x='Részvétel, %',y='LMP részesedése a haza lists szavazatokból')+
        #annotate("text", x = c(62,75), y = c(103,130), label = c("50%+", "2/3+"),color='red',size=7)+
        theme(plot.title = element_text( color="#666666", face="bold", size=22, hjust=0.5)) +
        theme(axis.title = element_text( color="#666666", face="bold", size=16))+
        theme(axis.text = element_text( color="#666666", face="bold", size=12)) 

