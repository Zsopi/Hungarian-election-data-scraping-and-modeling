#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(data.table)
library(dplyr)
library(ggplot2)

#setting locale for accented character handling
ifelse(.Platform$OS.type == 'windows', 
       Sys.setlocale(category = 'LC_ALL', locale = 'Hungarian'), 
       Sys.setlocale(category = 'LC_ALL', locale = 'hu_HU.utf8'))

#reading in scraped election data from my Github page
valker_result_df<-data.table(read.csv('https://raw.githubusercontent.com/Zsopi/Hungarian-election-data-scraping-and-modeling/master/valker_results.csv'))


#define function
get_seats_const_fidesz_plus_coord<-function(voting_share=61.26499,J_to_M,J_to_F,J_to_L,M_to_J,M_to_L,L_to_M,L_to_J) {
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
        baseline_winners<-c('FIDESZ_KDNP','MSZP_STB','JOBBIK','LMP')[apply(tt_hypo[,FIDESZ_egy:LMP_egy], 1, function(z){order(z, decreasing=TRUE)[1]})]
        
        #calculating alternative individual district outcome in case of one opposition candidate stepdowns
        tt_hypo[,`:=`(
                MSZP_egy_alt=MSZP_egy+round(J_to_M*JOBBIK_egy+L_to_M*LMP_egy),
                JOBBIK_egy_alt=JOBBIK_egy+round(M_to_J*MSZP_egy+L_to_J*LMP_egy),
                LMP_egy_alt=LMP_egy+round(M_to_L*MSZP_egy+J_to_L*JOBBIK_egy),
                
                #these are calculated to take into account the potential switch from Jobbik to Fidesz
                FIDESZ_egy_alt_c=FIDESZ_egy,
                MSZP_egy_alt_c=MSZP_egy+round(J_to_M*JOBBIK_egy+L_to_M*LMP_egy-J_to_F*JOBBIK_egy),
                JOBBIK_egy_alt_c=JOBBIK_egy+round(M_to_J*MSZP_egy+L_to_J*LMP_egy),
                LMP_egy_alt_c=LMP_egy+round(M_to_L*MSZP_egy+J_to_L*JOBBIK_egy-J_to_F*JOBBIK_egy)
        )]
        
        alt_winner<-c('FIDESZ_KDNP','MSZP_STB','JOBBIK','LMP')[apply(tt_hypo[,FIDESZ_egy_alt_c:LMP_egy_alt_c],1,which.max)]
        alt_second<-c('FIDESZ_KDNP','MSZP_STB','JOBBIK','LMP')[apply(tt_hypo[,FIDESZ_egy_alt_c:LMP_egy_alt_c], 1, function(z){order(z, decreasing=TRUE)[2]})]
        alt_third<-c('FIDESZ_KDNP','MSZP_STB','JOBBIK','LMP')[apply(tt_hypo[,FIDESZ_egy_alt_c:LMP_egy_alt_c], 1, function(z){order(z, decreasing=TRUE)[3]})]
        
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
        
        tt_listpct_adj<-tt_listpct_adj[1,MAS_lst_frm:=0]
        
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
        
        vote_ratio_summary<-data.table(FIDESZ_KDNP=0,
                                       MSZP_STB=0,
                                       Jobbik=0,
                                       LMP=0,
                                       Egyéb=0)
        vote_ratio_summary[1,]<-round(tt_listvotes/sum(tt_listvotes)*100,2)
                
                
        seats_summary<-data.table(
                Változó=c('Listás képviselőhelyek száma','Egyéni képviselőhelyek száma','Képviselők száma összesen'),
                FIDESZ_KDNP=(rep(0L,3)),
                MSZP_STB=(rep(0L,3)),
                Jobbik=(rep(0L,3)),
                LMP=(rep(0L,3)),
                Egyéb=(rep(0L,3))
        )
        
       seats_summary[1,2:6]<-seats[,FIDESZ_lst:MAS_lst]
       seats_summary[2,2:6]<-seats[,FIDESZ_egy:MAS_egy]
       seats_summary[3,2:6]<-seats[,FIDESZ_sum:MAS_sum]
       
        margin_over_FIDESZ<-cbind(tt_hypo[,MEGYE:SZEKHELY],baseline_winner=baseline_winners,alt_winner,alt_second,alt_third,tt_hypo[,FIDESZ_egy_alt_c:LMP_egy_alt_c])
        margin_over_FIDESZ[,MSZP_egyedul_mrg:=MSZP_egy_alt_c-FIDESZ_egy_alt_c]
        margin_over_FIDESZ[,JOBBIK_egyedul_mrg:=JOBBIK_egy_alt_c-FIDESZ_egy_alt_c]
        margin_over_FIDESZ[,LMP_egyedul_mrg:=LMP_egy_alt_c-FIDESZ_egy_alt_c]
        
        margin_over_FIDESZ[,OPP_max:=apply (margin_over_FIDESZ[,MSZP_egyedul_mrg:LMP_egyedul_mrg],1,function(z){z[order (z,decreasing=T)[1]]}) ]
        
        margin_over_FIDESZ<- margin_over_FIDESZ[order(OPP_max,decreasing=T),]
        
        margin_over_FIDESZ[,colnames(margin_over_FIDESZ[,FIDESZ_egy_alt_c:LMP_egy_alt_c]):=NULL]
        
        margin_over_FIDESZ<-margin_over_FIDESZ[baseline_winner=='FIDESZ_KDNP'&alt_winner!='FIDESZ_KDNP']
        
        margin_over_FIDESZ<-margin_over_FIDESZ[,names(margin_over_FIDESZ[,MSZP_egyedul_mrg:OPP_max]):=lapply(.SD, as.integer),.SDcols=MSZP_egyedul_mrg:OPP_max]

        setnames(margin_over_FIDESZ,old=names(margin_over_FIDESZ[,baseline_winner:OPP_max]),new=c('Alapeset győztes','Koordinációnál legesélyesebb','Koordinációnál második legesélyesebb','Koordinációnál harmadik legesélyesebb', 'MSZP marzsa egyedül induláskor','Jobbik marzsa egyedül induláskor','LMP marzsa egyedül induláskor','Ellenzék maximum marzsa'))
        
        out<-list(vote_ratio_summary,seats_summary,margin_over_FIDESZ)
        return(out)
        
        
        #####
}



# Define UI for application that draws tables
ui <- fluidPage(
   
   # Application title
   titlePanel("2014-es alternatív választási eredmény szimuláció"),
   
   # Sidebar with a slider input for party preferences
   sidebarLayout(
      sidebarPanel(
              inputPanel(
                      
                      actionButton("update", HTML("Lássuk az eredményt!<br/>(Számold újra!)")),
                      numericInput("voting_share", label = "Részvételi arány, %",value=61,min = 0,max = 100),
                      helpText("A modell feltételezi, hogy a 61% feletti részvétel esetén a többletszavazók kizárólag az ellenzékre szavaznak"),
                      h4("Átszavazás akkor, ha nincs saját jelölt és egy ellenzéki jelölt indul a körzetben, %"),
                      numericInput("J_to_M", label = "Jobbik szavazók az MSZP-re",value=50,min = 0,max = 100),
                      numericInput("J_to_F", label = "Jobbik szavazók a Fidesz-re",value=30,min = 0,max = 100),
                      numericInput("J_to_L", label = "Jobbik szavazók az LMP-re",value=50,min = 0,max = 100),
                      numericInput("M_to_J", label = "MSZP szavazók a Jobbikra",value=50,min = 0,max = 100),
                      numericInput("M_to_L", label = "MSZP szavazók az LMP-re",value=50,min = 0,max = 100),
                      numericInput("L_to_M", label = "LMP szavazók az MSZP-re",value=50,min = 0,max = 100),
                      numericInput("L_to_J", label = "LMP szavazók a Jobbikra",value=50,min = 0,max = 100)
                      
                      
              )
              
      ),
      
      # Show the generated tables
      mainPanel(
              h4("Listás szavazati arányok"),
              tableOutput("vote_ratio_summary"),
              h4("Parlament összetétele, ellenzék egésze számára optimális koordináció esetén (kerekítés miatt nem feltétlenül 199 az összlétszám)"),
              tableOutput("parlament"),
              h4("Átbillenthető körzetek (a marzsok a Fidesz-KDNP-hez viszonyított szavazati különbséget jelentik)"),
              tableOutput("competitive_districts")
              
      )
   )
)

# Define server logic required to calculate tables
server <- function(input, output) {
        
        #J_to_M<-eventReactive(input$update, {input$J_to_M/100})
        #J_to_F<-eventReactive(input$update, {input$J_to_F/100})
        #J_to_L<-eventReactive(input$update, {input$J_to_L/100})
        #M_to_J<-eventReactive(input$update, {input$M_to_J/100})
        #M_to_L<-eventReactive(input$update, {input$M_to_L/100})
        #L_to_M<-eventReactive(input$update, {input$L_to_M/100})
        #L_to_J<-eventReactive(input$update, {input$L_to_J/100})
        
        out<-eventReactive(input$update, {

                get_seats_const_fidesz_plus_coord(voting_share=input$voting_share,J_to_M=input$J_to_M/100,J_to_F=input$J_to_F/100,J_to_L=input$J_to_L/100,M_to_J=input$M_to_J/100,M_to_L=input$M_to_L/100,L_to_M=input$L_to_M/100,L_to_J=input$L_to_J/100)})
        
        output$vote_ratio_summary<-renderTable({
                out()[[1]]
        })
        
   output$parlament <- renderTable({
           out()[[2]]
   })
      
   output$competitive_districts <- renderTable({
           out()[[3]]
   }) 

}

# Run the application 
shinyApp(ui = ui, server = server)

