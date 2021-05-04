
# 1.load packages and functions to create confidence intervals
library(tidyverse,warn.conflicts = F)
library(pander,warn.conflicts = F)
library(broom)

# create confidence interval based on analysis with age group- cohort interaction (see below) and the cohort. 
# res_name is either Positive, Hospitalization, Severe, or Death
createCI <- function(analysis,res_name,cohort){
  names = c(paste0("age_category16-39:cohort",cohort),
            paste0("age_category40-49:cohort",cohort),
            paste0("age_category50-59:cohort",cohort),
            paste0("age_category60-69:cohort",cohort),
            paste0("age_category70-79:cohort",cohort),
            paste0("age_category80+:cohort",cohort))
  
  df <- broom::tidy(analysis) %>% 
    filter(term %in% names)
  
  df <- df %>% 
    mutate(Age = if(n()==6){
      c("16-39","40-49","50-59","60-69","70-79","80+")
    } else {c("60-69","70-79","80+")},
    est = formatC( round(1-exp(estimate),3)*100,format='f', digits=1 ),
    lower = formatC(round(pmax(0,1-exp(estimate+1.96*std.error)),3)*100,format='f', digits=1 ),
    upper = formatC(round(1-exp(estimate-1.96*std.error),3)*100,format='f', digits=1 ),
    res= paste0(est,"\\% [",lower,", ",upper,"]")) %>% 
    select(Age,res)
  
  #           res= paste0(est,"\\% [",lower,"\\%,",upper,"\\%]"))
  colnames(df) <- c("Age",res_name)
  if(nrow(df)== 3){
    df0 = tibble(Age = c("16-39","40-49","50-59"),res = rep(c("---"),3))
    colnames(df0) <- c("Age",res_name)
    df <- rbind(df0,df)
  }
  return(df)
  
}

# create confidence interval based on analysis with age group- cohort interaction (see below) and the cohort. 
# length is the format of the output; either short, long, or num for only the estimate.

createCItotal <- function(analysis,cohort, length = "long"){
  df <- broom::tidy(analysis) %>%
    filter(term == paste0("cohort",cohort) ) %>% 
    mutate(
      # take out the reference group
      #est = formatC( round(1-exp(estimate),3)*100,format='f', digits=1 ),
      est = round(1-exp(estimate),3)*100,
      lower = formatC(round(1-exp(estimate+1.96*std.error),3)*100,format='f', digits=1 ),
      upper = formatC(round(1-exp(estimate-1.96*std.error),3)*100,format='f', digits=1 ))
  
  if(length == "long"){
    df <- mutate(df,res= paste0(est,"\\% (CI: [",lower,", ",upper,"])"))
  } else if  (length == "short"){
    df <- mutate(df,res= paste0(est,"\\% [",lower,", ",upper,"]"))
  } else if(length == "num"){
    df <- mutate(df,res =round(est)) 
  }
  
  
  return(df$res)
  
}


# 2. load table and create sub-tables 
final_table_demo <- readRDS("./final_table_demo.RDS")
final_table_older <-  filter(final_table_demo,age_category %in% c("60-69","70-79","80+"))
final_table_not_recovered <- filter(final_table_demo, cohort %in% c(0,"1A","1B",2))
final_table_older_not_recovered <-  filter(final_table_not_recovered,age_category %in% c("60-69","70-79","80+"))







# 3. Run the analyses. analysis1/analysis2 is with/without age_group - cohort interaction
analysis1 <- glm(numevents ~  sex +age_category +city_risk +pcr +cohort+ offset(log(person_days)), 
                 family="poisson", data=final_table_demo)
analysis2 <- glm(numevents ~-1 +sex +age_category +city_risk + pcr +age_category:cohort+ offset(log(person_days)), 
                 family="poisson", data=final_table_demo)


analysis1_hosp <- glm(numevents_hosp ~sex +age_category +city_risk + pcr +cohort+ offset(log(person_days)), 
                      family="poisson", data=final_table_demo)
analysis2_hosp <- glm(numevents_hosp ~ -1 +sex +age_category +city_risk + pcr + age_category:cohort+ offset(log(person_days)), 
                      family="poisson", data=final_table_demo)


analysis1_severe <- glm(numevents_severe ~sex +age_category +city_risk + pcr +cohort+ offset(log(person_days)), 
                        family="poisson", data=final_table_older)
analysis2_severe <- glm(numevents_severe ~ -1 +sex +age_category  +city_risk +pcr + age_category:cohort+ offset(log(person_days)), 
                        family="poisson", data=final_table_older)


analysis1_death <- glm(numevents_death ~sex +age_category +city_risk + pcr +cohort+ offset(log(person_days)), 
                       family="poisson", data=final_table_older_not_recovered)
analysis2_death <- glm(numevents_death ~ -1+ sex +age_category+city_risk + pcr +age_category:cohort+ offset(log(person_days)), 
                       family="poisson", data=final_table_older_not_recovered)





#4. Create confidence interval based on cohort.
df <- createCI(analysis2,res_name= "Positive",cohort = "2")
df <- inner_join(df,createCI(analysis2_hosp,res_name= "Hospitalized",cohort = "2"),by="Age")
df <- inner_join(df,createCI(analysis2_severe,res_name= "Severe",cohort = "2"),by="Age")
df <- inner_join(df,createCI(analysis2_death,res_name= "Death",cohort = "2"),by="Age")
df <- rbind(df, tibble(Age = "Overall", 
                       Positive =  createCItotal(analysis1,"2","short"),
                       Hospitalized = createCItotal(analysis1_hosp,"2","short"),
                       Severe =  createCItotal(analysis1_severe,"2","short"),
                       Death =  createCItotal(analysis1_death,"2","short")))

#pander(df)
