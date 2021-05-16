

#### Data Workflow Final Project
### 2020/05/12
### Wenrui Huang

### A. Importing Data
rm(list=ls())

setwd("~/Desktop/DataWorkflow/Final project")
#library(readr)
library(tidyverse)
#library(foreign)
#library(dplyr)
library(readxl)
data_Deming_2008_0217 <- read_excel("data_Deming_2008_0217.xlsx")


### B.Sample eligibility

#Rule is 5 years old by 1990, so that they will be 19 by 2004;
#Next restrict the sample to families with at least 2 age-eligible children;
#Finally restrict to families where at least one (but not all) children were in Head Start;

# Rule for preschool participation:  
# substitute the PPVT age (ie age at test) when the original variable is unavailable, and also substitutes past or future age variables from other survey years (plus or minus 24 months).
# a. Replace missing age values with PPVT Age or age reported from the nearest survey year instead

# years: 86, 88, 90, 92, 94, 96, 98, 100, 102, 104
aly_age1 <- select(data_Deming_2008_0217, Age_Mo86:Age_Mo104, PPVTAge86:PPVTAge104, MotherID, ChildID, Res86:Res90, Res104)
aly_age1$Age_Mo86_r <- aly_age1$Age_Mo86
aly_age1$Age_Mo88_r <- aly_age1$Age_Mo88
aly_age1$Age_Mo90_r <- aly_age1$Age_Mo90
aly_age1$Age_Mo92_r <- aly_age1$Age_Mo92
aly_age1$Age_Mo94_r <- aly_age1$Age_Mo94
aly_age1$Age_Mo96_r <- aly_age1$Age_Mo96
aly_age1$Age_Mo98_r <- aly_age1$Age_Mo98
aly_age1$Age_Mo100_r <- aly_age1$Age_Mo100
aly_age1$Age_Mo102_r <- aly_age1$Age_Mo102
aly_age1$Age_Mo104_r <- aly_age1$Age_Mo104

aly_age1$Age_Mo86=ifelse(!(is.na(aly_age1$Age_Mo86)),  aly_age1$Age_Mo86,  aly_age1$PPVTAge86)  
aly_age1$Age_Mo88=ifelse(!(is.na(aly_age1$Age_Mo88)),  aly_age1$Age_Mo88,  aly_age1$PPVTAge88)  
aly_age1$Age_Mo90=ifelse(!(is.na(aly_age1$Age_Mo90)),  aly_age1$Age_Mo90,  aly_age1$PPVTAge90)  
aly_age1$Age_Mo92=ifelse(!(is.na(aly_age1$Age_Mo92)),  aly_age1$Age_Mo92,  aly_age1$PPVTAge92)  
aly_age1$Age_Mo94=ifelse(!(is.na(aly_age1$Age_Mo94)),  aly_age1$Age_Mo94,  aly_age1$PPVTAge94)  
aly_age1$Age_Mo96=ifelse(!(is.na(aly_age1$Age_Mo96)),  aly_age1$Age_Mo96,  aly_age1$PPVTAge96)  
aly_age1$Age_Mo98=ifelse(!(is.na(aly_age1$Age_Mo98)),  aly_age1$Age_Mo98,  aly_age1$PPVTAge98)  
aly_age1$Age_Mo100=ifelse(!(is.na(aly_age1$Age_Mo100)),aly_age1$Age_Mo100, aly_age1$PPVTAge100)  
aly_age1$Age_Mo102=ifelse(!(is.na(aly_age1$Age_Mo102)),aly_age1$Age_Mo102, aly_age1$PPVTAge102)  
aly_age1$Age_Mo104=ifelse(!(is.na(aly_age1$Age_Mo104)),aly_age1$Age_Mo104, aly_age1$PPVTAge104)  

aly_age1 <- aly_age1 %>% mutate(Age_Mo86= case_when(
  is.na(Age_Mo86) & Age_Mo88>=25 & !(is.na(Age_Mo88)) ~ Age_Mo88-24,
                                                TRUE  ~ Age_Mo86
))

aly_age1 <- aly_age1 %>% mutate(Age_Mo88= case_when(
  is.na(Age_Mo88) & !(is.na(Age_Mo86)) ~ Age_Mo86+24,
  TRUE  ~ Age_Mo88
))
aly_age1 <- aly_age1 %>% mutate(Age_Mo88= case_when(
  is.na(Age_Mo88) & Age_Mo90>=25 & !(is.na(Age_Mo90)) ~ Age_Mo90-24,
  TRUE  ~ Age_Mo88
))

aly_age1 <- aly_age1 %>% mutate(Age_Mo90= case_when(
  is.na(Age_Mo90) & !(is.na(Age_Mo88)) ~ Age_Mo88+24,
  TRUE  ~ Age_Mo90
))
aly_age1 <- aly_age1 %>% mutate(Age_Mo90= case_when(
  is.na(Age_Mo90) & Age_Mo92>=25 & !(is.na(Age_Mo92)) ~ Age_Mo92-24,
  TRUE  ~ Age_Mo90
))

aly_age1 <- aly_age1 %>% mutate(Age_Mo92= case_when(
  is.na(Age_Mo92) & !(is.na(Age_Mo90)) ~ Age_Mo90+24,
  TRUE  ~ Age_Mo92
))
aly_age1 <- aly_age1 %>% mutate(Age_Mo92= case_when(
  is.na(Age_Mo92) & Age_Mo94>=25 & !(is.na(Age_Mo94)) ~ Age_Mo94-24,
  TRUE  ~ Age_Mo92
))

aly_age1 <- aly_age1 %>% mutate(Age_Mo94= case_when(
  is.na(Age_Mo94) & !(is.na(Age_Mo92)) ~ Age_Mo92+24,
  TRUE  ~ Age_Mo94
))
aly_age1 <- aly_age1 %>% mutate(Age_Mo94= case_when(
  is.na(Age_Mo94) & Age_Mo96>=25 & !(is.na(Age_Mo96)) ~ Age_Mo96-24,
  TRUE  ~ Age_Mo94
))

aly_age1 <- aly_age1 %>% mutate(Age_Mo96= case_when(
  is.na(Age_Mo96) & !(is.na(Age_Mo94)) ~ Age_Mo94+24,
  TRUE  ~ Age_Mo96
))
aly_age1 <- aly_age1 %>% mutate(Age_Mo96= case_when(
  is.na(Age_Mo96) & Age_Mo98>=25 & !(is.na(Age_Mo98)) ~ Age_Mo98-24,
  TRUE  ~ Age_Mo96
))

aly_age1 <- aly_age1 %>% mutate(Age_Mo98= case_when(
  is.na(Age_Mo98) & !(is.na(Age_Mo96)) ~ Age_Mo96+24,
  TRUE  ~ Age_Mo98
))
aly_age1 <- aly_age1 %>% mutate(Age_Mo98= case_when(
  is.na(Age_Mo98) & Age_Mo100>=25 & !(is.na(Age_Mo100)) ~ Age_Mo100-24,
  TRUE  ~ Age_Mo98
))

aly_age1 <- aly_age1 %>% mutate(Age_Mo100= case_when(
  is.na(Age_Mo100) & !(is.na(Age_Mo98)) ~ Age_Mo98+24,
  TRUE  ~ Age_Mo100
))
aly_age1 <- aly_age1 %>% mutate(Age_Mo100= case_when(
  is.na(Age_Mo100) & Age_Mo102>=25 & !(is.na(Age_Mo102)) ~ Age_Mo102-24,
  TRUE  ~ Age_Mo100
))

aly_age1 <- aly_age1 %>% mutate(Age_Mo102= case_when(
  is.na(Age_Mo102) & !(is.na(Age_Mo100)) ~ Age_Mo100+24,
  TRUE  ~ Age_Mo102
))
aly_age1 <- aly_age1 %>% mutate(Age_Mo102= case_when(
  is.na(Age_Mo102) & Age_Mo104>=25 & !(is.na(Age_Mo104)) ~ Age_Mo104-24,
  TRUE  ~ Age_Mo102
))

aly_age1 <- aly_age1 %>% mutate(Age_Mo104= case_when(
  is.na(Age_Mo104) & !(is.na(Age_Mo102)) ~ Age_Mo102+24,
  TRUE  ~ Age_Mo104
))



# Create age in years (rather than months) variable*;
aly_age1$age_year86_r=ifelse(aly_age1$Age_Mo86_r<12, 0, NA)
aly_age1$age_year86=ifelse(aly_age1$Age_Mo86<12, 0, NA)

aly_age1$age_year88_r=ifelse(aly_age1$Age_Mo88_r<12, 0, NA)
aly_age1$age_year88=ifelse(aly_age1$Age_Mo88<12, 0, NA)

aly_age1$age_year90_r=ifelse(aly_age1$Age_Mo90_r<12, 0, NA)
aly_age1$age_year90=ifelse(aly_age1$Age_Mo90<12, 0, NA)

aly_age1$age_year92_r=ifelse(aly_age1$Age_Mo92_r<12, 0, NA)
aly_age1$age_year92=ifelse(aly_age1$Age_Mo92<12, 0, NA)

aly_age1$age_year94_r=ifelse(aly_age1$Age_Mo94_r<12, 0, NA)
aly_age1$age_year94=ifelse(aly_age1$Age_Mo94<12, 0, NA)

aly_age1$age_year96_r=ifelse(aly_age1$Age_Mo96_r<12, 0, NA)
aly_age1$age_year96=ifelse(aly_age1$Age_Mo96<12, 0, NA)

aly_age1$age_year98_r=ifelse(aly_age1$Age_Mo98_r<12, 0, NA)
aly_age1$age_year98=ifelse(aly_age1$Age_Mo98<12, 0, NA)

aly_age1$age_year100_r=ifelse(aly_age1$Age_Mo100_r<12, 0, NA)
aly_age1$age_year100=ifelse(aly_age1$Age_Mo100<12, 0, NA)

aly_age1$age_year102_r=ifelse(aly_age1$Age_Mo102_r<12, 0, NA)
aly_age1$age_year102=ifelse(aly_age1$Age_Mo102<12, 0, NA)

aly_age1$age_year104_r=ifelse(aly_age1$Age_Mo104_r<12, 0, NA)
aly_age1$age_year104=ifelse(aly_age1$Age_Mo104<12, 0, NA)


aly_age1$age_year86 <-0
for (i in 1:37) {
  aly_age1$age_year86 <-
  ifelse (aly_age1$Age_Mo86>= 12*i & aly_age1$ Age_Mo86<(12*i+12), i, aly_age1$age_year86 )
}

aly_age1$age_year86_r <-0
for (i in 1:37) {
  aly_age1$age_year86_r <-
    ifelse (aly_age1$Age_Mo86_r>= 12*i & aly_age1$Age_Mo86_r<(12*i+12),i, aly_age1$age_year86_r )
}

aly_age1$age_year88 <-0
for (i in 1:37) {
  aly_age1$age_year88 <-
    ifelse (aly_age1$Age_Mo88>= 12*i & aly_age1$ Age_Mo88<(12*i+12), i, aly_age1$age_year88 )
}

aly_age1$age_year88_r <-0
for (i in 1:37) {
  aly_age1$age_year88_r <-
    ifelse (aly_age1$Age_Mo88_r>= 12*i & aly_age1$Age_Mo88_r<(12*i+12),i, aly_age1$age_year88_r )
}

aly_age1$age_year90 <-0
for (i in 1:37) {
  aly_age1$age_year90 <-
    ifelse (aly_age1$Age_Mo90>= 12*i & aly_age1$ Age_Mo90<(12*i+12), i, aly_age1$age_year90 )
}

aly_age1$age_year90_r <-0
for (i in 1:37) {
  aly_age1$age_year90_r <-
    ifelse (aly_age1$Age_Mo90_r>= 12*i & aly_age1$Age_Mo90_r<(12*i+12),i, aly_age1$age_year90_r )
}

aly_age1$age_year92 <-0
for (i in 1:37) {
  aly_age1$age_year92 <-
    ifelse (aly_age1$Age_Mo92>= 12*i & aly_age1$ Age_Mo92<(12*i+12), i, aly_age1$age_year92 )
}

aly_age1$age_year92_r <-0
for (i in 1:37) {
  aly_age1$age_year92_r <-
    ifelse (aly_age1$Age_Mo92_r>= 12*i & aly_age1$Age_Mo92_r<(12*i+12),i, aly_age1$age_year92_r )
}

aly_age1$age_year94 <-0
for (i in 1:37) {
  aly_age1$age_year94 <-
    ifelse (aly_age1$Age_Mo94>= 12*i & aly_age1$ Age_Mo94<(12*i+12), i, aly_age1$age_year94 )
}

aly_age1$age_year94_r <-0
for (i in 1:37) {
  aly_age1$age_year94_r <-
    ifelse (aly_age1$Age_Mo94_r>= 12*i & aly_age1$Age_Mo94_r<(12*i+12),i, aly_age1$age_year94_r )
}

aly_age1$age_year96 <-0
for (i in 1:37) {
  aly_age1$age_year96 <-
    ifelse (aly_age1$Age_Mo96>= 12*i & aly_age1$ Age_Mo96<(12*i+12), i, aly_age1$age_year96 )
}

aly_age1$age_year96_r <-0
for (i in 1:37) {
  aly_age1$age_year96_r <-
    ifelse (aly_age1$Age_Mo96_r>= 12*i & aly_age1$Age_Mo96_r<(12*i+12),i, aly_age1$age_year96_r )
}


aly_age1$age_year98 <-0
for (i in 1:37) {
  aly_age1$age_year98 <-
    ifelse (aly_age1$Age_Mo98>= 12*i & aly_age1$ Age_Mo98<(12*i+12), i, aly_age1$age_year98 )
}

aly_age1$age_year98_r <-0
for (i in 1:37) {
  aly_age1$age_year98_r <-
    ifelse (aly_age1$Age_Mo98_r>= 12*i & aly_age1$Age_Mo98_r<(12*i+12),i, aly_age1$age_year98_r )
}


aly_age1$age_year100 <-0
for (i in 1:37) {
  aly_age1$age_year100 <-
    ifelse (aly_age1$Age_Mo100>= 12*i & aly_age1$ Age_Mo100<(12*i+12), i, aly_age1$age_year100 )
}

aly_age1$age_year100_r <-0
for (i in 1:37) {
  aly_age1$age_year100_r <-
    ifelse (aly_age1$Age_Mo100_r>= 12*i & aly_age1$Age_Mo100_r<(12*i+12),i, aly_age1$age_year100_r )
}


aly_age1$age_year102 <-0
for (i in 1:37) {
  aly_age1$age_year102 <-
    ifelse (aly_age1$Age_Mo102>= 12*i & aly_age1$ Age_Mo102<(12*i+12), i, aly_age1$age_year102 )
}

aly_age1$age_year102_r <-0
for (i in 1:37) {
  aly_age1$age_year102_r <-
    ifelse (aly_age1$Age_Mo102_r>= 12*i & aly_age1$Age_Mo102_r<(12*i+12),i, aly_age1$age_year102_r )
}

aly_age1$age_year104 <-0
for (i in 1:37) {
  aly_age1$age_year104 <-
    ifelse (aly_age1$Age_Mo104>= 12*i & aly_age1$ Age_Mo104<(12*i+12), i, aly_age1$age_year104 )
}

aly_age1$age_year104_r <-0
for (i in 1:37) {
  aly_age1$age_year104_r <-
    ifelse (aly_age1$Age_Mo104_r>= 12*i & aly_age1$Age_Mo104_r<(12*i+12),i, aly_age1$age_year104_r )
}


# Create dummy for sample eligibility, by year
# Minimum of 2 siblings age 54 months or more, in each sample year 
# Want to account in some way for the fact that people are surveyed at different times of the year
# But also don't want to include siblings who subsequently enroll
# Chose 4 years, 6 months - these kids are almost certainly too old to enroll if they are not already in the program
# Estimates in the paper are not sensitive to this rule


library(tidyverse)
library(haven)
library(sjlabelled)
select <- dplyr::select

# Load stata resutls to compare
df_correct <- read_csv("stata_final.csv")
df <- aly_age1
df_attempt <- df %>%
  
  select(MotherID, ChildID, starts_with("Age_Mo")) %>%
  select(MotherID, ChildID, matches("86|88|90")) %>%
  
  # Pivot the child ages, so we can perform operations on them as a group (similar to Stata's foreach y in 86 88 90)
  pivot_longer(cols = starts_with("Age_Mo"), names_to = "child", values_to = "age_mo") %>%
  mutate(gr_than_47_not_miss = ifelse(age_mo > 47 & !is.na(age_mo), 1, 0)) %>%
  group_by(MotherID, child) %>%
  mutate(count_children = sum(gr_than_47_not_miss)) %>%
  ungroup() %>%

  # If a child satisfies the "greater than 47 and not missing requirement", 
  # then set their Elig to the number of children that satisfies the requirements
  mutate(Elig = ifelse(gr_than_47_not_miss == 1, count_children, NA)) %>%

  # Recode Elig (lines 92-97 in Stata)
  mutate(Elig = ifelse(Elig == 1, 0,
                ifelse(Elig > 1 & !is.na(Elig), 1,
                ifelse(Elig != 1, 0, 0)))) %>%
  
  # The above code still has NAs, so change the NAs to 0s
  mutate(Elig = ifelse(is.na(Elig), 0, Elig)) %>%
  
  # Pivot the dataset back to wide
  select(MotherID, ChildID, child, Elig) %>%
  mutate(child = str_replace(child, "Age_Mo", "Elig")) %>%
  pivot_wider(names_from = "child", values_from = "Elig")

# This is the Stata output 
df_correct2 <- df_correct %>%
  select(MotherID, ChildID, starts_with("Elig")) %>%
  arrange(MotherID, ChildID)
df_correct2
df_correct2 %>%
  view("correct")

df_attempt2 <- df_attempt %>%
  select(MotherID, ChildID, Elig86_r, Elig86, Elig88_r, Elig88, Elig90_r, Elig90) %>%
  setNames(names(df_correct2)) %>%
  arrange(MotherID, ChildID) %>%
  sjlabelled::remove_all_labels() %>% # remove Stata labels/formats
  zap_formats() %>%
  as_tibble()
df_attempt2
df_attempt2 %>%
  view("attempt")

dplyr::all_equal(df_attempt2, df_correct2)

# Check frequencies 
table(df_correct2$Elig1_86)
table(df_attempt2$Elig1_86)
table(df_correct2$Elig1_88)
table(df_attempt2$Elig1_88)


#Exclude small number of kids who died prior to eligibility*;
#combine orginal age data and newly produced eligblity data

df2 <- left_join(aly_age1, df_correct2, by="ChildID")

df3<-df2 %>%
  mutate(Dead86=ifelse(Res86==8 & !is.na(Res86),1,0))%>%
  mutate(Dead88=ifelse(Res88==8 & !is.na(Res88),1,0))%>%
  mutate(Dead90=ifelse(Res90==8 & !is.na(Res90),1,0))%>%
  mutate(Elig1_86=ifelse(Dead86==1 & !is.na(Elig1_86), NA, Elig1_86))%>%
  mutate(Elig1_88=ifelse(Dead88==1 & !is.na(Elig1_88), NA, Elig1_88))%>%
  mutate(Elig1_90=ifelse(Dead90==1 & !is.na(Elig1_90), NA, Elig1_90))%>%
  mutate(Elig2_86=ifelse(Dead86==1 & !is.na(Elig2_86), NA, Elig2_86))%>%
  mutate(Elig2_88=ifelse(Dead88==1 & !is.na(Elig2_88), NA, Elig2_88))%>%
  mutate(Elig2_90=ifelse(Dead90==1 & !is.na(Elig2_90), NA, Elig2_90))%>%
  mutate(Deceased=ifelse(Res104==8 & !is.na(Res104),1,0)) 


#Create Head Start, Other Preschool, and No Preschool categories*;
#Create variables equal to one if respondent ever indicated enrollment in HS or other preschools in 1988-1992* ;
# Question was not asked in 1986*;

df_prg<- data_Deming_2008_0217%>%
  select(ChildID, Ever_HS88:Ever_HS104,Ever_Preschool88:Ever_Preschool104, HowLongLim90, HowLong_HS88:HowLong_HS104)

df4<- left_join(df3, df_prg, by="ChildID")%>%
  mutate(HS1_90=case_when(Elig1_90==1 & (Ever_HS88==1 | Ever_HS90==1)~1,
                          Elig1_90==1 & (Ever_HS88==0 | Ever_HS90==0)~0))%>%
  mutate(HS2_90=case_when(Elig2_90==1 & (Ever_HS88==1 | Ever_HS90==1)~1,
                          Elig2_90==1 & (Ever_HS88==0 | Ever_HS90==0)~0))%>%
  mutate(Pre1_90=case_when(Elig1_90==1 & (Ever_Preschool88==1 | Ever_Preschool90==1)~1,
                           Elig1_90==1 & (Ever_Preschool88==0 | Ever_Preschool90==0)~0))%>%
  mutate(Pre2_90=case_when(Elig2_90==1 & (Ever_Preschool88==1 | Ever_Preschool90==1)~1,
                           Elig2_90==1 & (Ever_Preschool88==0 | Ever_Preschool90==0)~0))%>%
  mutate(HS1_90=ifelse(Elig1_90==1 & (HS1_90==0 | is.na(HS1_90)), 0, HS1_90))%>%
  mutate(HS2_90=ifelse(Elig2_90==1 & (HS2_90==0 | is.na(HS2_90)), 0, HS2_90))%>%
  mutate(Pre1_90=ifelse(Elig1_90==1 & (Pre1_90==0 | is.na(Pre1_90)), 0, Pre1_90))%>%
  mutate(Pre2_90=ifelse(Elig2_90==1 & (Pre2_90==0 | is.na(Pre2_90)), 0, Pre2_90))%>%

   # code "yes" responses to both HS and preschool as Head Start (Rule 3 does it the other way)*;
  mutate(Pre1_90=ifelse(HS1_90==1, 0, Pre1_90))%>%
  mutate(Pre2_90=ifelse(HS2_90==1, 0, Pre2_90))%>%
  mutate(None1_90=ifelse(Elig1_90==1, 1, 0))%>%
  mutate(None1_90=ifelse(Elig1_90==0 & !is.na(Elig1_90==0), NA, None1_90))%>%
  mutate(None1_90=ifelse((HS1_90==1 | Pre1_90==1) , 0, 1))%>%
  mutate(None1_90=ifelse(is.na(Elig1_90), 0, None1_90))%>%
  mutate(None2_90=ifelse(Elig2_90==1, 1, 0))%>%
  mutate(None2_90=ifelse(Elig2_90==0 & !is.na(Elig2_90==0), NA, None2_90))%>%
  mutate(None2_90=ifelse((HS2_90==1 | Pre2_90==1) , 0, 1))%>%
  mutate(None2_90=ifelse(is.na(Elig2_90), 0, None2_90))

table(df4$None1_90)
table(df4$None2_90)


# Alternative participation definition - code inconsistent responses across years as missing or zeros*;
# Also code as zero those who report being in Head Start for less than 3 months*;

df4$HS3_90<-df4$HS2_90 
df4$Pre3_90<-df4$Pre2_90 
df4$None3_90<-df4$None2_90

df4<- df4%>%
  mutate(l=ifelse((Ever_HS88==1 & !is.na(Ever_HS88) )&(Ever_HS90==0 & !is.na(Ever_HS90) ),1,0))%>%
           mutate(m=ifelse((HowLong_HS88==1 & !is.na(HowLong_HS88) ) |(HowLong_HS90==1 & !is.na(HowLong_HS90)), 1, 0))%>%
                    mutate(HS3_90=ifelse((l==1| m==1) & !is.na(HS3_90), 0, HS3_90)) %>%
  mutate(k=ifelse((Ever_Preschool88==1 &!is.na(Ever_Preschool88))  & (Ever_Preschool90==0 & !is.na(Ever_Preschool90)),1,0))%>%
  mutate(Pre3_90=ifelse(k==1 & !is.na(Pre3_90), 0, Pre3_90)) %>%
  mutate(None3_90=ifelse((HS3_90==0 & !is.na(HS3_90) )& (Pre3_90==0& !is.na(Pre3_90)) & !is.na(None3_90), 1, None3_90))

table(df4$HS3_90)
table(df4$Pre3_90)
table(df4$None3_90)

#Create dummy for "fixed effects" sample - families where HS participation varies across siblings


df <- aly_age1
# Start recoding
df_fx1 <- df %>%
  
  select(MotherID, ChildID, starts_with("Age_Mo")) %>%
  select(MotherID, ChildID, matches("86|88|90")) %>%
  
  # Pivot the child ages
  pivot_longer(cols = starts_with("Age_Mo"), names_to = "child", values_to = "age_mo") %>%
  
  # For every child in the dataset, let 1 = age is greater than 47 and not missing
  mutate(gr_than_47_not_miss = ifelse(age_mo > 47 & !is.na(age_mo), 1, 0)) %>%
  
  # Count the number of children in each age category that satisfies the above requirement
  group_by(MotherID, child) %>%
  mutate(count_children = sum(gr_than_47_not_miss)) %>%
  ungroup() %>%
  
  # If a child satisfies the "greater than 47 and not missing requirement", 
  # then set their Elig to the number of children that satisfies the requirements
  mutate(Elig = ifelse(gr_than_47_not_miss == 1, count_children, NA)) %>%
  
  #  NAs to 0s
  mutate(Elig = ifelse(is.na(Elig), 0, Elig)) %>%
  
  # Pivot the dataset back to wide
  select(MotherID, ChildID, child, Elig) %>%
  mutate(child = str_replace(child, "Age_Mo", "Elig")) %>%
  pivot_wider(names_from = "child", values_from = "Elig")


df_fx2 <- df_fx1 %>%
  select(MotherID, ChildID, Elig86_r, Elig86, Elig88_r, Elig88, Elig90_r, Elig90) %>%
  setNames(names(df_correct2)) %>%
  arrange(MotherID, ChildID) %>%
  sjlabelled::remove_all_labels() %>% # remove Stata labels/formats
  zap_formats() %>%
  as_tibble() 

df_fx3 <- df_fx2 %>%
  mutate(Elig3_86=Elig2_86)%>%
  mutate(Elig3_88=Elig2_88)%>%
  mutate(Elig3_90=Elig2_90)%>%
  
  rename(Num1Elig86= Elig1_86,
         Num2Elig86= Elig2_86,
         Num3Elig86= Elig3_86,
         Num1Elig88= Elig1_88,
         Num2Elig88= Elig2_88,
         Num3Elig88= Elig3_88,
         Num1Elig90= Elig1_90,
         Num2Elig90= Elig2_90,
         Num3Elig90= Elig3_90
         )
df_fx3
table(df_fx3$Num2Elig88)



#For each family, count the number of kids in each category (HS, Pre, None)*
# Then set equal to missing if all kids in the family are in the same category (and thus ineligible)*;


df_fx4 <- left_join(df4, df_fx3, by="ChildID")

df_fxtm<- df_fx4%>%
  select(MotherID, ChildID, starts_with("HS"), starts_with("Pre"), starts_with("None"), contains("Elig90")) %>%
  pivot_longer(cols = HS1_90:None3_90, names_to = "kid_cat", values_to = "kid_cat_vals") %>%
  pivot_longer(cols = contains("Elig90"), names_to = "elig", values_to = "elig_vals") %>%
  group_by(MotherID, kid_cat) %>%
  mutate(temp = sum(kid_cat_vals, na.rm = T)) %>%
  mutate(temp = ifelse(temp == elig_vals, NA, temp))



df_fxtm<- df_fx4%>%
  select(MotherID, ChildID, starts_with("HS"))%>%
  pivot_longer(cols = starts_with("HS"), names_to = "child2", values_to = "count") %>%
  mutate(temp1 = ifelse(count==1, 1, 0))%>%
  group_by(MotherID, child2) %>%
  mutate(temp2=sum(temp1))%>%
  ungroup()%>%
mutate(temp = ifelse(temp1 == 1, temp2, NA) )%>%
mutate(temp = ifelse(is.na(temp), 0, temp)) %>%


# Pivot the dataset back to wide
select(MotherID, ChildID, child2, temp) %>%
  mutate(child2 = str_replace(child2, "HS", "temp")) %>%
  pivot_wider(names_from = "child2", values_from = "temp")
 
      
df_fx5 <- left_join(df_fx4, df_fxtm, by="ChildID") %>%
         mutate(temp1_90=ifelse(Num1Elig90==temp1_90, NA, temp1_90))%>%
         mutate(temp1_90=ifelse(Num2Elig90==temp2_90, NA, temp1_90))%>%
         mutate(temp1_90=ifelse(Num3Elig90==temp3_90, NA, temp1_90))%>%


#table(df_fx5$temp1_90)

mutate(HS1_FE90=ifelse(!is.na(temp1_90), 1, NA))%>%
mutate(Pre1_FE90=ifelse(!is.na(temp2_90), 1, NA)) %>%        
mutate(None1_FE90=ifelse(!is.na(temp3_90), 1, NA)) %>%  
mutate(HS1_FE90=ifelse(Pre1_FE90==1 |None1_FE90==1 ), 0, NA) %>%
mutate(Pre1_FE90=ifelse(HS1_FE90==1 |None1_FE90==1 ), 0, NA) %>%
mutate(None1_FE90=ifelse(HS1_FE90==1 |Pre1_FE90==1 ), 0, NA) %>%
  
  mutate(HS2_FE90=ifelse(!is.na(temp1_90), 1, NA))%>%
  mutate(Pre2_FE90=ifelse(!is.na(temp2_90), 1, NA)) %>%        
  mutate(None2_FE90=ifelse(!is.na(temp3_90), 1, NA)) %>%  
  mutate(HS2_FE90=ifelse(Pre2_FE90==1 |None2_FE90==1 ), 0, NA) %>%
  mutate(Pre2_FE90=ifelse(HS2_FE90==1 |None2_FE90==1 ), 0, NA) %>%
  mutate(None2_FE90=ifelse(HS2_FE90==1 |Pre2_FE90==1 ), 0, NA) %>%
  
  mutate(HS3_FE90=ifelse(!is.na(temp1_90), 1, NA))%>%
  mutate(Pre3_FE90=ifelse(!is.na(temp2_90), 1, NA)) %>%        
  mutate(None3_FE90=ifelse(!is.na(temp3_90), 1, NA)) %>%  
  mutate(HS3_FE90=ifelse(Pre3_FE90==1 |None3_FE90==1 ), 0, NA) %>%
  mutate(Pre3_FE90=ifelse(HS3_FE90==1 |None3_FE90==1 ), 0, NA) %>%
  mutate(None3_FE90=ifelse(HS3_FE90==1 |Pre3_FE90==1 ), 0, NA) %>%
  
  mutate(PreK=ifelse(HS2_90==1, 1, 0))%>%
  mutate(PreK=ifelse(Pre2_90==1, 2, 0))%>%
  mutate(PreK=ifelse(None2_90==1, 3, 0))%>%
  mutate(PreK_FE=ifelse(HS2_FE90==1, 1, 0))%>%
  mutate(PreK_FE=ifelse(Pre2_FE90==1, 2, 0))%>%
  mutate(PreK_FE=ifelse(None2_FE90==1, 3, 0))%>%
  
  

# END SAMPLE ELIGIBILITY SECTION
# Until now all consistent with stata results
################################################

# C. Create Covariates in Tables 1 
library(readr)
df_tb1 <- read_csv("elgi2.csv")

df_tb1 <- df_tb1%>% 
  mutate(Hispanic=ifelse(Race_Child==1 & !is.na(Race_Child==1),1,0)
  )%>%
  mutate(Black   =ifelse(Race_Child==2 & !is.na(Race_Child==1),1,0)
  )%>% 
  mutate(White   =ifelse(Race_Child==3 & !is.na(Race_Child==1),1,0)
  )%>%
  mutate(NonBlack=ifelse(Race_Child!=2 & !is.na(Race_Child==1),1,0)
         )%>%

  #Generate basic demographic variables in Table 1
  #Permanent Income, Mother<HS, Mother some college, Maternal AFQT, Grandmother's Education
  #Create yearly income in constant 2004 dollars, as well as "permanent income" measure
  mutate(NetFamInc78=NetFamInc78*2.82, 
         NetFamInc79=NetFamInc79*2.54,
         NetFamInc80=NetFamInc80*2.24,
         NetFamInc81=NetFamInc81*2.03,
         NetFamInc82=NetFamInc82*1.90,
         NetFamInc83=NetFamInc83*1.85,
         NetFamInc84=NetFamInc84*1.78,
         NetFamInc85=NetFamInc85*1.71,
         NetFamInc86=NetFamInc86*1.68,
         NetFamInc87=NetFamInc87*1.62,
         NetFamInc88=NetFamInc88*1.55,
         NetFamInc89=NetFamInc89*1.48,
         NetFamInc90=NetFamInc90*1.41,
         NetFamInc91=NetFamInc91*1.35,
         NetFamInc92=NetFamInc92*1.31,
         NetFamInc93=NetFamInc93*1.27,
         NetFamInc95=NetFamInc95*1.21,
         NetFamInc97=NetFamInc97*1.15,
         NetFamInc99=NetFamInc99*1.10,
         NetFamInc101=NetFamInc101*1.04)

library(data.table)
df_mean <- select(df_tb1, ChildID, starts_with("NetFamInc"))
setDT(df_mean)
mean1<-df_mean[,.(PermInc = rowMeans ((.SD), na.rm=TRUE)), by = ChildID ]
df_tb2<- left_join(df_tb1, mean1,  by="ChildID")

df_max_grademo <-select(df_tb1, ChildID, starts_with("HighGrade_Moth"))
setDT(df_max_grademo)

library(matrixStats)
df_max_grademo$MothED = rowMaxs(as.matrix(df_max_grademo[,c(-1)]),na.rm=TRUE )
df_tb2<- left_join(df_tb1, mean1,  by="ChildID") 


df_tb2<- left_join(df_tb1, mean1,  by="ChildID") %>%
  left_join(df_tb2, df_max_grademo$MothED, by="ChildID")%>% 
  mutate(lnPermInc=log(PermInc))%>%
  mutate(PermInc_std=scale(df_tb2$PermInc))

df_tb2$AgeAFQT<-df_tb2$AFQT_Pct81_REV

require(dplyr)
df_tb3 <-
  left_join(df_tb2,
            df_max_grademo %>% dplyr::select(MothED,ChildID),
            by = "ChildID")%>%
  mutate(MomDropout=ifelse(MothED<12 & MothED>=0 & !is.na(MothED),1, 0)
  )%>%
  mutate(MomDropout=ifelse(is.na(MothED)& !is.na(MomDropout), NA, MomDropout))%>%
  mutate(MomHS=ifelse(MothED==12, 1, 0))%>%
  mutate(MomHS=ifelse(is.na(MothED), NA, MomHS))%>%
  mutate(MomSomeColl=ifelse(MothED>=13 & MothED<=20 & !is.na(MothED), 1, 0))%>%
  mutate(MomSomeColl=ifelse(is.na(MothED), NA, MomSomeColl))%>%


# Create age-adjusted maternal AFQT score
  
mutate(AgeAFQT=case_when(Age_Mom79==14~AgeAFQT*(35.60881/28.79544),
                         Age_Mom79==15~AgeAFQT*(35.60881/32.86273),
                         Age_Mom79==16~AgeAFQT*(35.60881/32.86273),
                         Age_Mom79==17~AgeAFQT*(35.60881/36.3544),
                         Age_Mom79==18~AgeAFQT*(35.60881/33.45777),
                         Age_Mom79==19~AgeAFQT*(35.60881/36.84) ,
                         Age_Mom79==20~AgeAFQT*(35.60881/41.84536),
                         Age_Mom79==21~AgeAFQT*(35.60881/40.95177),
                         Age_Mom79==22~AgeAFQT*(35.60881/42.82069),
                         TRUE~AgeAFQT))

df_tb3<-df_tb3%>%       
  mutate(AgeAFQT_std=scale(df_tb3$AgeAFQT))
 
df_tb3$AgeAFQT_std<- as.numeric(df_tb3$AgeAFQT_std)   
  
  
table(df_tb3$MomDropout)
table(df_tb3$Black)
table(df_tb3$NonBlack)
table(df_tb3$AgeAFQT)
summary(df_tb3$AgeAFQT)
summary(df_tb3$AgeAFQT_std)
sum(is.na(df_tb3$AgeAFQT_std))
sum(is.na(df_tb3$AgeAFQT))


#For a small number of mothers, AFQT score is missing - impute missings for use as a covariate later
install.packages("mice")
library(mice)

df_imAFQT<-df_tb3 %>%
  select(ChildID, AgeAFQT_std, Black, Hispanic, Age_Moth_Birth )
impAFQT_std <- mice(df_imAFQT, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(impAFQT_std)
sum(is.na(impAFQT_std$AgeAFQT_std))

#But use non-imputed values only for Table 1










################################
# D.Creating Final Table 1
################################
  
# LIST OF VARIABLES FOR TABLE 1 - PermInc MomDropout MomBA AgeAFQT_std HighGrade_GMom79

### Prepare sample for table
black<- df_tb3[ which(df_tb3$Black==1),]
nonblack<- df_tb3[ which(df_tb3$NonBlack==1),]




table1_var<-xtabs (~ Black + NonBlack + PermInc + MomDropout + MomSomeColl + AgeAFQT_std + HighGrade_GMom79 +PreK +PreK_FE, data=df_tb3)

library(apaTables)
library(dplyr)
table(df_tb3$MomDropout)

MomDropout1<- filter(df_tb3, Black==1)
#goggles.women <- filter(goggles,gender=="Female")


apa.d.table(iv =PreK , dv =MomDropout , 
            data = MomDropout1, 
            filename = "Table1_APA.doc", landscape = TRUE)




apa.2way.table(iv =PreK , dv =MomDropout , data = df_tb3, 
               filename = "Table1_APA.docx", landscape=TRUE)


install.packages("vcd")
library(vcd)
table1_var<-xtabs (~ Black + NonBlack + PermInc + MomDropout + MomSomeColl + AgeAFQT_std + HighGrade_GMom79 +PreK +PreK_FE, data=df_tb3)
         

library(apaTables)
apa.2way.table(iv1=Black,iv2=NonBlack, dv=Prek,data=table1_var,filename="table1try.doc", landscape=TRUE)



table1.1 = table1_var("PreK" = rep(c("HS", "Pre", "None"), 20), 
                      "MomDropout" = rep(c("Yes", "No"), 20))  ## Data Generation
ftable(table1.1$MomDropout, table1.1$PreK)

set.seed(42)
install.packages("qwraps2")
library(qwraps2)



table(df_tb3$PreK)


table_varl<-
  list("Permannent income"=
         list("mean(sd)"=  ~qwraps2::mean_sd(PermInc)),
    "Mother below high school" =
          list("mean(sd)"= ~qwraps2::mean_sd(MomDropout))
  )

whole1 <- summary_table(dplyr::group_by(df_tb3, PreK, Black==1), table_varl)

       
  whole1<-summary.table(df_tb3,  table_varl)
  whole
       
       )