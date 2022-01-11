# re-doing some previous STATA analysis in R because STATA is a pain to access through Berkeley

# load libraries
library(tidyverse)
library(haven)
library(psych)

# read in data
mydata <- read_dta('~/Desktop/Worrell/SGoyalImp.dta')
mydata2 <- read_dta('~/Desktop/Worrell/SGoyalImp_RaceMatching.dta')
view(mydata)

# check stats reported in paper to make sure they line up
sum(mydata$race=='Afr Am')
sum(mydata$race=='White')

# check about using last mp gap (it's higher!)
mean(mydata$cum_gpa, na.rm = TRUE)
mean(mydata$lastmp_g, na.rm=TRUE)

# diagnostics for GPA by race
mydata %>% 
  filter(race=='Afr Am') %>% 
  pull(cum_gpa) %>% 
  mean(na.rm=TRUE)
mydata %>% 
  filter(race=='Afr Am') %>% 
  pull(lastmp_g) %>% 
  mean(na.rm=TRUE)

mydata %>% 
  filter(race=='White') %>% 
  pull(cum_gpa) %>% 
  mean(na.rm=TRUE)
mydata %>% 
  filter(race=='White') %>% 
  pull(lastmp_g) %>% 
  mean(na.rm=TRUE)

### GENERATE COMPOSITE VARIABLES (except ANX and DEP)
mydata$empathy <- (mydata$emp1 + mydata$emp2 + 
                     mydata$emp3 + mydata$emp4 + 
                     mydata$emp5 + mydata$emp6 + 
                     mydata$emp7) / 7
mydata$genSCon <-(mydata$GSC1 + mydata$gsc2 + 
                    mydata$gsc3 + mydata$gsc4 + 
                    mydata$gsc5 + mydata$gsc6 + 
                    mydata$gsc7 + mydata$gsc8) / 8
mydata$satLife <- (mydata$sat1 + mydata$sat2 + 
                     mydata$sat3 + mydata$sat4 + 
                     mydata$sat5) / 5
mydata$proSoBeh <- (mydata$pro1 + mydata$pro2 + 
                      mydata$pro3 + mydata$pro4 + 
                      mydata$pro5 + mydata$pro6 + 
                      mydata$pro7) / 7
mydata$acadMot <- (mydata$sww1 + mydata$sww2 + 
                     mydata$sww3 + mydata$sww4 + 
                     mydata$sww5 + mydata$sww6 + 
                     mydata$sww7 + mydata$sww8 + 
                     mydata$sww9) / 9
mydata$anxiety <- (mydata$ranx1 + mydata$ranx2 + 
                     mydata$ranx3 + mydata$ranx4 + 
                     mydata$ranx5 + mydata$ranx6 + 
                     mydata$ranx7) / 7
mydata$depression <- (mydata$rdep8 + mydata$rdep9 + 
                        mydata$rdep10 + mydata$rdep11 + 
                        mydata$rdep12 + mydata$rdep13 + 
                        mydata$rdep14 + mydata$rdep15 + 
                        mydata$rdep16 + mydata$rdep17 + 
                        mydata$rdep18) / 11
mydata$optimism <- (mydata$ropt1 + mydata$opt2 + 
                      mydata$ropt3 + mydata$opt4 + 
                      mydata$ropt5 + mydata$opt6 + 
                      mydata$opt7 + mydata$ropt8) / 8

### GENERATE ANX AND DEP VARIABLES

# see what decimal values there are from having imputing the data
# want to check if anything will round down to 0
remove <- c(1.0, 2.0, 3.0, 4.0)
setdiff(mydata$ranx3, remove)

## STATA CODE FOR REFERENCE
#gen anx1=1 if ranx1==4
#replace anx1=2 if ranx1==3
#replace anx1=3 if ranx1==2
#replace anx1=4 if ranx1==1

## MANUAL WAY TO DO IT IN R-- CTRL+SHIFT+C to comment all
# mydata$anx1 <- # imputed data can be decimals
# ifelse(mydata$ranx1>3 & mydata$ranx1<=4, 1,
#       ifelse(mydata$ranx1>2 & mydata$ranx1<=3, 2,
#             ifelse(mydata$ranx1>1 & mydata$ranx1<=2, 3,
#                   1))) # puts 1 if value >0 and <=1

## CLEAN WAY TO DO IT IN R
replaceValues <- function(.x) { #.x just naming convention
  dplyr::case_when(
    .x==4 ~ 1,
    .x==3 ~ 2,
    .x==2 ~ 3,
    .x==1 ~ 4,
    .x==0 ~ 4 # just in case imputed value <.5 got rounded
  )
}

data_rounded <- mydata %>% # imputed data can be non-integer, so converting everything to integer
  mutate(across(c(starts_with('ranx')), round)) %>% 
  mutate(across(c(starts_with('rdep')), round))

data_anx_dep <- data_rounded %>% 
  dplyr::mutate(
    across(
      c(starts_with('ranx'), starts_with('rdep')),
      .fns = list(~replaceValues(.)),
      .names = '{fn}_{col}' # will by default be 1_ranx# etc.
    )
  ) %>% 
  dplyr::rename_with(
    ~substr(., 4, 10), # 10 is an overshoot to catch the whole tail of the string
    .cols = starts_with('1_r') # will catch 1_ranx# and 1_rdep#
  )


data_anx_dep$correctAnxiety <-  (data_anx_dep$anx1 + data_anx_dep$anx2 + data_anx_dep$anx3 + 
                             data_anx_dep$anx4 + data_anx_dep$anx5 + data_anx_dep$anx6 + 
                             data_anx_dep$anx7) / 7
data_anx_dep$correctDepression <-  (data_anx_dep$dep8 + data_anx_dep$dep9 + data_anx_dep$dep10 + 
                                data_anx_dep$dep11 + data_anx_dep$dep12 + data_anx_dep$dep13 + 
                                data_anx_dep$dep14 + data_anx_dep$dep15 + data_anx_dep$dep16 + 
                                data_anx_dep$dep17 + data_anx_dep$dep18) / 11

### DESCRIPTIVE STATISTICS

# descriptive table
data_analyze <- data_anx_dep %>% 
  select(cum_gpa, empathy, genSCon, satLife, proSoBeh, acadMot, correctAnxiety, correctDepression, 
         optimism, anxiety, depression)

data_analyze %>% 
  psych::describe() %>% 
  print()

data_analyze %>% 
  psych::describe() %>% 
  print()

# to compare values by race
data_analyze %>% 
  psych::describeBy(data_anx_dep$race) %>% 
  print()

# to compare values by gender
data_analyze %>% 
  psych::describeBy(data_anx_dep$gender) %>% 
  print()

# correlation matrix
M <- data_analyze %>%
  cor(method='pearson', use = "complete.obs") %>% 
  round(2)

M2 <- Hmisc::rcorr(as.matrix(data_analyze))

corrplot(M2$r,  
         type = "lower",
         addCoef.col = "black", # Add coefficient of correlation #Text label color and rotation
         # Combine with significance level
         p.mat = M2$P, sig.level = 0.001,  
         # hide correlation coefficient on the principal diagonal
         diag = FALSE 
)

### CHECK SEWB CONSTRUCT SCORES FOR WHITE VS BLACK

race_analyze <- data_anx_dep %>% # descriptive table
  select(race, cum_gpa, empathy, genSCon, satLife, 
         proSoBeh, optimism, correctAnxiety, 
         correctDepression, acadMot) 

## check if normally distributed, for t-test vs. Mann Whitney
shapiro.test(data_analyze$empathy)
df.shapiro <- data_analyze %>% 
  lapply(function(x) x[!is.na(x)]) %>% 
  lapply(as.numeric) %>% 
  lapply(shapiro.test) # at this point, class(df.shapiro) = 'list'
for (item in df.shapiro) {
  print(item$p.value)
} # all non-normal

## use Mann-Whitney to compare values for black vs. white

empathyA <- race_analyze %>% 
  filter(race=='Afr Am') %>% 
  select(empathy) %>% 
  unlist()
empathyW <- race_analyze %>% 
  filter(race=='White') %>% 
  select(empathy) %>% 
  unlist()

wilcox.test(empathyA, empathyW) #says wilcox but is really Mann-Whitney

for (i in 2:ncol(race_analyze)) {
  dfA <- race_analyze %>% 
    filter(race=='Afr Am')
  varA <- as.numeric(unlist(dfA[, i]))
  
  dfW <- race_analyze %>% 
    filter(race=='White')
  varW <- as.numeric(unlist(dfW[, i]))
  
  print(names(race_analyze)[i])
  print(mean(varA))
  print(mean(varW))
  print(wilcox.test(varA, varW))
  
  #paste(class(varA))
  #print(mean(as.numeric(unlist(varA))))
}

# DO REGRESSION
data_scaled <- lapply(data_analyze, scale) # to get standardized coefficients
reg <- lm(formula = cum_gpa ~ empathy + genSCon + satLife + proSoBeh + 
     optimism + acadMot + correctAnxiety + correctDepression, 
   data = data_analyze)
summary(reg)
plot(reg$residuals) # should look random

# regression by race
reg_race_A <- race_analyze %>% 
  filter(race=='Afr Am')
reg_race_A[,-1] <- lapply(reg_race_A[,-1], scale)
regA <- lm(formula = cum_gpa ~ empathy + genSCon + satLife + proSoBeh + 
             optimism + acadMot + correctAnxiety + correctDepression, 
           data = reg_race_A)
summary(regA)

reg_race_W <- race_analyze %>% 
  filter(race=='White')
regW <- lm(formula = cum_gpa ~ empathy + genSCon + satLife + proSoBeh + 
             optimism + acadMot + correctAnxiety + correctDepression, 
           data = reg_race_W)
summary(regW)

## STATA CODE FOR REFERENCE
# //regression, GPA as dependent variable
# reg cum_gpa empathy genSCon satLife proSoBeh optimism acadMot anxiety depression
# //regression with standardized beta
# reg cum_gpa empathy genSCon satLife proSoBeh optimism acadMot anxiety depression ,b
# reg cum_gpa empathy genSCon satLife proSoBeh optimism acadMot originalAnxiety originalDepression ,b
# 
# //regression with genSCon as dependent variable
# reg genSCon empathy satLife proSoBeh optimism acadMot originalAnxiety originalDepression ,b
# reg genSCon cum_gpa empathy satLife proSoBeh optimism acadMot originalAnxiety originalDepression ,b