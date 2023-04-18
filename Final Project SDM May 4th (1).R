rm(list=ls())
setwd("C:/Users/caitl/Downloads")
Adult20 <- read.csv("adult20.csv")
head(Adult20)
names(Adult20)
View(Adult20)


#Depression Level: One of our response variables

Adult20$DEPLEVEL_A <- ifelse(Adult20$DEPLEVEL_A == 1 ,'1',ifelse(Adult20$DEPLEVEL_A == 2,'3',
                     ifelse(Adult20$DEPLEVEL_A == 3,'2',ifelse(Adult20$DEPLEVEL_A == 7,'99',ifelse(Adult20$DEPLEVEL_A == 8,'99',
                      ifelse(Adult20$DEPLEVEL_A == 9,'99','99'))))))

table(is.na(Adult20$DEPLEVEL_A))
#Lots of missing values...can use other depression variables to help us.

Adult20$DEPLEVEL_A[(is.na(Adult20$DEPLEVEL_A)==TRUE)&(Adult20$DEPFREQ_A==5)] = 0
Adult20$DEPLEVEL_A[(is.na(Adult20$DEPLEVEL_A)==TRUE)&(Adult20$DEPEV_A==2)] = 0
Adult20$DEPLEVEL_A[(is.na(Adult20$DEPLEVEL_A))==TRUE] = 99

#Take out the unknowns, there are less than 100 and we want to use DEPLEVEL_A as
#a numeric variable.
table(Adult20$DEPLEVEL_A)
Adult20 = subset(Adult20, (DEPLEVEL_A != 99))
table(Adult20$DEPLEVEL_A)
Adult20$DEPLEVEL_A = as.numeric(Adult20$DEPLEVEL_A)

#Anxiety Level: One of our response variables

Adult20$ANXLEVEL_A = ifelse(Adult20$ANXLEVEL_A==1, '1', ifelse(Adult20$ANXLEVEL_A==2, '3',
                            ifelse(Adult20$ANXLEVEL_A==3, '2', ifelse(Adult20$ANXLEVEL_A==7, '99',
                            ifelse(Adult20$ANXLEVEL_A==8, '99', ifelse(Adult20$ANXLEVEL_A==9, '99', '99'))))))

table(is.na(Adult20$ANXLEVEL_A))
#Lots of missing values...can use other anxiety variables to help us.

Adult20$ANXLEVEL_A[Adult20$ANXFREQ_A==5] = 0
Adult20$ANXLEVEL_A[(is.na(Adult20$ANXLEVEL_A)==TRUE)&(Adult20$ANXEV_A==2)] = 0
Adult20$ANXLEVEL_A[(is.na(Adult20$ANXLEVEL_A))==TRUE] = 99

#Take out the unknowns, there are less than 100 and we want to use ANXLEVEL_A as
#a numeric variable.
table(Adult20$ANXLEVEL_A)
Adult20 = subset(Adult20, (ANXLEVEL_A != 99))
table(Adult20$ANXLEVEL_A)
Adult20$ANXLEVEL_A = as.numeric(Adult20$ANXLEVEL_A)


#Plotting / Data Exploration
#Subset to only known data

#Depression vs. Anxiety
onlyknownAnxDep = subset(Adult20, (ANXLEVEL_A != 99)&(DEPLEVEL_A!=99))
counts = table(onlyknownAnxDep$ANXLEVEL_A, onlyknownAnxDep$DEPLEVEL_A)
barplot(counts, main="Anxiety Level vs. Depression Level",
        xlab="Anxiety Level", col=c("darkblue","red", "green", "yellow"),
        legend = rownames(counts))
onlyknownAnxDep$ANXLEVEL_A = as.numeric(onlyknownAnxDep$ANXLEVEL_A)
onlyknownAnxDep$DEPLEVEL_A = as.numeric(onlyknownAnxDep$DEPLEVEL_A)
cor.test(onlyknownAnxDep$ANXLEVEL_A, onlyknownAnxDep$DEPLEVEL_A)
#There is a 53% correlation between anxiety level and depression level.


#Anxiety vs. Sleep
onlyknownAnxSleep = subset(Adult20, (ANXLEVEL_A!= 99)& (SLPHOURS_A!=97)& (SLPHOURS_A!=98) & (SLPHOURS_A!=99))
counts = table(onlyknownAnxSleep$ANXLEVEL_A, onlyknownAnxSleep$SLPHOURS_A)
barplot(counts, main="Anxiety Level vs. Hours of Sleep",
        xlab="Sleep Hours", col=c("darkblue","red", "green", "yellow"),
        legend = rownames(counts))
onlyknownAnxSleep$ANXLEVEL_A = as.numeric(onlyknownAnxSleep$ANXLEVEL_A)
onlyknownAnxSleep$SLPHOURS_A = as.numeric(onlyknownAnxSleep$SLPHOURS_A)
cor.test(onlyknownAnxSleep$ANXLEVEL_A, onlyknownAnxSleep$SLPHOURS_A)
#No correlation between hours of sleep and anxiety


#Depression vs. Sleep
onlyknownDepSleep = subset(Adult20, (DEPLEVEL_A!= 99)& (SLPHOURS_A!=97)& (SLPHOURS_A!=98) & (SLPHOURS_A!=99))
counts = table(onlyknownDepSleep$DEPLEVEL_A, onlyknownDepSleep$SLPHOURS_A)
barplot(counts, main="Depression Level vs. Hours of Sleep",
        xlab="Sleep Hours", col=c("darkblue","red", "green", "yellow"),
        legend = rownames(counts))
onlyknownDepSleep$DEPLEVEL_A = as.numeric(onlyknownDepSleep$DEPLEVEL_A)
onlyknownDepSleep$SLPHOURS_A = as.numeric(onlyknownDepSleep$SLPHOURS_A)
cor.test(onlyknownDepSleep$DEPLEVEL_A, onlyknownDepSleep$SLPHOURS_A)
#No correlation between hours of sleep and depression




#Actionable variables

#Nicotine. Creating column for use of nicotine related products (cigarettes, e-cigarettes, smokeless tobacco, pipes, cigars)
Adult20$nicotine = 3
table(Adult20$nicotine)
Adult20$nicotine[Adult20$SMKCIGST_A==1 | Adult20$SMKCIGST_A==2 | Adult20$SMKCIGST_A==4 | Adult20$SMKCIGST_A==5] = 'Currently'
Adult20$nicotine[Adult20$SMKCIGST_A==4] = 'Never'
Adult20$nicotine[Adult20$SMKCIGST_A==3] = 'Formerly'
Adult20$nicotine[Adult20$SMKCIGST_A==9] = 'Unknown'
table(Adult20$nicotine, Adult20$SMKCIGST_A)
table(Adult20$nicotine, Adult20$SMKECIGST_A)
Adult20$nicotine[Adult20$SMKECIGST_A==1 | Adult20$SMKECIGST_A==4] = 'Currently'
Adult20$nicotine[(Adult20$SMKECIGST_A==2)&(Adult20$SMKCIGST_A==4)] = 'Formerly'
table(Adult20$nicotine, Adult20$SMOKELSCUR_A)
Adult20$nicotine[Adult20$SMOKELSCUR_A==1 | Adult20$SMOKELSCUR_A==2] = 'Currently'
table(Adult20$nicotine, Adult20$PIPECUR_A)
Adult20$nicotine[Adult20$PIPECUR_A==1 | Adult20$PIPECUR_A==2] = 'Currently'
table(Adult20$nicotine, Adult20$CIGARCUR_A)
Adult20$nicotine[Adult20$CIGARCUR_A==1 | Adult20$CIGARCUR_A==2] = 'Currently'
Adult20 = subset(Adult20, nicotine!='Unknown')
Adult20$nicotine <- as.factor(Adult20$nicotine)


#Walking
colnames(Adult20)[which(names(Adult20) == "WLKLEIS_A")] <- "walk"
table(Adult20$walk)
table(is.na(Adult20$walk))
Adult20$walk[Adult20$walk ==2] = 0
Adult20$walk[Adult20$walk ==7 | Adult20$walk ==8 | Adult20$walk ==9 | is.na(Adult20$walk) ==TRUE] = 'Unknown'
Adult20 = subset(Adult20, walk!='Unknown')



#Sleep
colnames(Adult20)[which(names(Adult20) == "SLPHOURS_A")] <- "sleep"
Adult20$sleep[Adult20$sleep==97 | Adult20$sleep==98] =99
table(Adult20$sleep)
Adult20 = subset(Adult20, sleep!=99)



#Exercise - Create new column which measures how physically active
colnames(Adult20)[which(names(Adult20) == "MODFREQW_A")] <- "ModExerciseFreq"
colnames(Adult20)[which(names(Adult20) == "VIGFREQW_A")] <- "VigExerciseFreq"
colnames(Adult20)[which(names(Adult20) == "STRFREQW_A")] <- "StrExerciseFreq"


Adult20$ModExerciseFreq[Adult20$ModExerciseFreq==94]=0
Adult20$ModExerciseFreq[Adult20$ModExerciseFreq==95]=29
Adult20$ModExerciseFreq[Adult20$ModExerciseFreq==96]=-1
Adult20$ModExerciseFreq[Adult20$ModExerciseFreq>96]=-2
table(Adult20$ModExerciseFreq)

Adult20$VigExerciseFreq[Adult20$VigExerciseFreq==94]=0
Adult20$VigExerciseFreq[Adult20$VigExerciseFreq==95]=29
Adult20$VigExerciseFreq[Adult20$VigExerciseFreq==96]=-1
Adult20$VigExerciseFreq[Adult20$VigExerciseFreq>96]=-2
table(Adult20$VigExerciseFreq)

Adult20$StrExerciseFreq[Adult20$StrExerciseFreq==94]=0
Adult20$StrExerciseFreq[Adult20$StrExerciseFreq==95]=29
Adult20$StrExerciseFreq[Adult20$StrExerciseFreq==96]=-1
Adult20$StrExerciseFreq[Adult20$StrExerciseFreq>96]=-2
table(Adult20$StrExerciseFreq)


#Exercising 0 times a week can be considered not active...
Adult20$Exercise[Adult20$ModExerciseFreq==0 | Adult20$VigExerciseFreq==0 | Adult20$StrExerciseFreq==0] = 'Not Active'
#Exercising 1-2 times a week can be considered lightly active...overrides Not Active
Adult20$Exercise[Adult20$ModExerciseFreq==1 | Adult20$ModExerciseFreq==2 | Adult20$VigExerciseFreq==1 | Adult20$VigExerciseFreq==2 | Adult20$StrExerciseFreq==1 | Adult20$StrExerciseFreq==2] = 'Lightly Active'
#Exercising 3-6 times a week can be considered moderately active...overrides Not Active and Lightly Active
Adult20$Exercise[((Adult20$ModExerciseFreq>2)&(Adult20$ModExerciseFreq<7)) | ((Adult20$VigExerciseFreq>2)&(Adult20$VigExerciseFreq<7)) | ((Adult20$StrExerciseFreq>2)&(Adult20$StrExerciseFreq<7))] = 'Moderately Active'
#Exercising 7 times or more a week can be considered very active...overrides Not Active, Lightly Active and Moderately active
Adult20$Exercise[Adult20$ModExerciseFreq>6 | Adult20$VigExerciseFreq>6 | Adult20$StrExerciseFreq>6] = 'Very active'
#Physically unable to to exercise...only this factor if labeled physically unable for all three types
Adult20$Exercise[(Adult20$ModExerciseFreq==-1)&(Adult20$VigExerciseFreq==-1)&(Adult20$StrExerciseFreq==-1)] = 'Not Physically Able'
#Unknown...only this factor if unknown for all three types
Adult20$Exercise[(Adult20$ModExerciseFreq==-2)&(Adult20$VigExerciseFreq==-2)&(Adult20$StrExerciseFreq==-2)] = 'Unknown'

Adult20 = subset(Adult20, Exercise!='Unknown')
table(Adult20$Exercise)
Adult20$Excercise = as.factor(Adult20$Exercise)



#Weight Loss
colnames(Adult20)[which(names(Adult20) == "NOWWGTPRG_A")] <- "weightloss"
Adult20$weightloss[Adult20$weightloss==2] = 0
Adult20$weightloss[Adult20$weightloss==7 | Adult20$weightloss==8 | Adult20$weightloss==9] = 'Unknown'
table(Adult20$weightloss)
Adult20 = subset(Adult20, weightloss!='Unknown')


#Drinking Status
colnames(Adult20)[which(names(Adult20) == "DRKSTAT_A")] <- "alcohol"
table(Adult20$alcohol)
Adult20$alcohol[Adult20$alcohol==1] = "Never"
Adult20$alcohol[Adult20$alcohol==2 | Adult20$alcohol==3 | Adult20$alcohol==4] = "Former"
Adult20$alcohol[Adult20$alcohol==5] = "Infrequent"
Adult20$alcohol[Adult20$alcohol==6] = "Light"
Adult20$alcohol[Adult20$alcohol==7 | Adult20$alcohol==9] = "Moderate"
Adult20$alcohol[Adult20$alcohol==8] = "Heavy"
Adult20$alcohol[Adult20$alcohol==10] = "Unknown"
table(Adult20$alcohol)
Adult20 = subset(Adult20, alcohol!='Unknown')
Adult20$alcohol = as.factor(Adult20$alcohol)


#BMI
colnames(Adult20)[which(names(Adult20) == "BMICAT_A")] <- "bmi"
Adult20$bmi[Adult20$bmi==1] = "Underweight"
Adult20$bmi[Adult20$bmi==2] = "Healthy Weight"
Adult20$bmi[Adult20$bmi==3] = "Overweight"
Adult20$bmi[Adult20$bmi==4] = "Obese"
Adult20$bmi[Adult20$bmi==9] = "Unknown"
table(Adult20$bmi)
Adult20 = subset(Adult20, bmi!='Unknown')
Adult20$bmi = as.factor(Adult20$bmi)

#Food Security
colnames(Adult20)[which(names(Adult20) == "FDSCAT4_A")] <- "FoodSecurity"
#Placeholders...
Adult20$FoodSecurity[Adult20$FoodSecurity==4] = -1
Adult20$FoodSecurity[Adult20$FoodSecurity==3] = -2
Adult20$FoodSecurity[Adult20$FoodSecurity==2] = -3
Adult20$FoodSecurity[Adult20$FoodSecurity==1] = -4
Adult20$FoodSecurity[Adult20$FoodSecurity==-1] = 1
Adult20$FoodSecurity[Adult20$FoodSecurity==-2] = 2
Adult20$FoodSecurity[Adult20$FoodSecurity==-3] = 3
Adult20$FoodSecurity[Adult20$FoodSecurity==-4] = 4
Adult20 = subset(Adult20, FoodSecurity!=8)
table(Adult20$FoodSecurity)

#Creating a new subset with all actionable variables...
Adult20Actionable = Adult20[, c("nicotine", "walk", "sleep", "Exercise", "weightloss", "alcohol", "bmi", "FoodSecurity", "ANXLEVEL_A", "DEPLEVEL_A")]
View(Adult20Actionable)
str(Adult20Actionable)
Adult20Actionable$walk = as.factor(Adult20Actionable$walk)
Adult20Actionable$Exercise = as.factor(Adult20Actionable$Exercise)
Adult20Actionable$weightloss = as.factor(Adult20Actionable$weightloss)



#Health Related Variables - Unchanging.

Adult20Health = Adult20[, c("HYPEV_A", "CHLEV_A", "CHDEV_A", "ANGEV_A", "MIEV_A", "STREV_A", 
                            "ASEV_A", "CANEV_A", "DIBEV_A", "COPDEV_A", "ARTHEV_A", "DEMENEV_A", 
                            "HOSPONGT_A", "THERA12M_A", "DEPLEVEL_A", "ANXLEVEL_A")]
names(Adult20Health)
colnames(Adult20Health)[which(names(Adult20Health) == "HYPEV_A")] <- "HYPERTENSION"
colnames(Adult20Health)[which(names(Adult20Health) == "CHLEV_A")] <- "HIGH_CHOLESTEROL"
colnames(Adult20Health)[which(names(Adult20Health) == "CHDEV_A")] <- "HEART_DISEASE"
colnames(Adult20Health)[which(names(Adult20Health) == "ANGEV_A")] <- "ANGINA"
colnames(Adult20Health)[which(names(Adult20Health) == "MIEV_A")] <- "HEART_ATTACK"
colnames(Adult20Health)[which(names(Adult20Health) == "STREV_A")] <- "STROKE"
colnames(Adult20Health)[which(names(Adult20Health) == "ASEV_A")] <- "ASTHMA"
colnames(Adult20Health)[which(names(Adult20Health) == "CANEV_A")] <- "CANCER"
colnames(Adult20Health)[which(names(Adult20Health) == "DIBEV_A")] <- "DIABETES"
colnames(Adult20Health)[which(names(Adult20Health) == "HOSPONGT_A")] <- "HOSPITAL_OVERNIGHT"
colnames(Adult20Health)[which(names(Adult20Health) == "THERA12M_A")] <- "THERAPY_NOT_MENTAL"
colnames(Adult20Health)[which(names(Adult20Health) == "COPDEV_A")] <- "LUNG_DISEASE"
colnames(Adult20Health)[which(names(Adult20Health) == "ARTHEV_A")] <- "ARTHRITIS"
colnames(Adult20Health)[which(names(Adult20Health) == "DEMENEV_A")] <- "DEMENTIA"


Adult20Health$HYPERTENSION[Adult20Health$HYPERTENSION==2]=0
Adult20Health$HYPERTENSION[Adult20Health$HYPERTENSION==7 |Adult20Health$HYPERTENSION==8 | Adult20Health$HYPERTENSION==9]=2
table(is.na(Adult20Health$HYPERTENSION))
table(Adult20Health$HYPERTENSION)

Adult20Health$HIGH_CHOLESTEROL[Adult20Health$HIGH_CHOLESTEROL==2]=0
Adult20Health$HIGH_CHOLESTEROL[Adult20Health$HIGH_CHOLESTEROL==7 |Adult20Health$HIGH_CHOLESTEROL==8 | Adult20Health$HIGH_CHOLESTEROL==9]=2
table(is.na(Adult20Health$HIGH_CHOLESTEROL))
table(Adult20Health$HIGH_CHOLESTEROL)

Adult20Health$HEART_DISEASE[Adult20Health$HEART_DISEASE==2]=0
Adult20Health$HEART_DISEASE[Adult20Health$HEART_DISEASE==7 |Adult20Health$HEART_DISEASE==8 | Adult20Health$HEART_DISEASE==9]=2
table(is.na(Adult20Health$HEART_DISEASE))
table

Adult20Health$ANGINA[Adult20Health$ANGINA==2]=0
Adult20Health$ANGINA[Adult20Health$ANGINA==7 |Adult20Health$ANGINA==8 | Adult20Health$ANGINA==9]=2
table(is.na(Adult20Health$ANGINA))

Adult20Health$HEART_ATTACK[Adult20Health$HEART_ATTACK==2]=0
Adult20Health$HEART_ATTACK[Adult20Health$HEART_ATTACK==7 |Adult20Health$HEART_ATTACK==8 | Adult20Health$HEART_ATTACK==9]=2
table(is.na(Adult20Health$HEART_ATTACK))

Adult20Health$STROKE[Adult20Health$STROKE==2]=0
Adult20Health$STROKE[Adult20Health$STROKE==7 |Adult20Health$STROKE==8 | Adult20Health$STROKE==9]=2
table(is.na(Adult20Health$STROKE))

Adult20Health$ASTHMA[Adult20Health$ASTHMA==2]=0
Adult20Health$ASTHMA[Adult20Health$ASTHMA==7 |Adult20Health$ASTHMA==8 | Adult20Health$ASTHMA==9]=2
table(is.na(Adult20Health$ASTHMA))

Adult20Health$CANCER[Adult20Health$CANCER==2]=0
Adult20Health$CANCER[Adult20Health$CANCER==7 |Adult20Health$CANCER==8 | Adult20Health$CANCER==9]=2
table(is.na(Adult20Health$CANCER))

Adult20Health$DIABETES[Adult20Health$DIABETES==2]=0
Adult20Health$DIABETES[Adult20Health$DIABETES==7 |Adult20Health$DIABETES==8 | Adult20Health$DIABETES==9]=2
table(is.na(Adult20Health$DIABETES))

Adult20Health$HOSPITAL_OVERNIGHT[Adult20Health$HOSPITAL_OVERNIGHT==2]=0
Adult20Health$HOSPITAL_OVERNIGHT[Adult20Health$HOSPITAL_OVERNIGHT==7 |Adult20Health$HOSPITAL_OVERNIGHT==8 | Adult20Health$HOSPITAL_OVERNIGHT==9]=2
table(is.na(Adult20Health$HOSPITAL_OVERNIGHT))

Adult20Health$THERAPY_NOT_MENTAL[Adult20Health$THERAPY_NOT_MENTAL==2]=0
Adult20Health$THERAPY_NOT_MENTAL[Adult20Health$THERAPY_NOT_MENTAL==7 |Adult20Health$THERAPY_NOT_MENTAL==8 | Adult20Health$THERAPY_NOT_MENTAL==9]=2
table(is.na(Adult20Health$THERAPY_NOT_MENTAL))

Adult20Health$LUNG_DISEASE[Adult20Health$LUNG_DISEASE==2]=0
Adult20Health$LUNG_DISEASE[Adult20Health$LUNG_DISEASE==7 |Adult20Health$LUNG_DISEASE==8 | Adult20Health$LUNG_DISEASE==9]=2
table(is.na(Adult20Health$LUNG_DISEASE))

Adult20Health$ARTHRITIS[Adult20Health$ARTHRITIS==2]=0
Adult20Health$ARTHRITIS[Adult20Health$ARTHRITIS==7 |Adult20Health$ARTHRITIS==8 | Adult20Health$ARTHRITIS==9]=2
table(is.na(Adult20Health$ARTHRITIS))

Adult20Health$DEMENTIA[Adult20Health$DEMENTIA==2]=0
Adult20Health$DEMENTIA[Adult20Health$DEMENTIA==7 |Adult20Health$DEMENTIA==8 | Adult20Health$DEMENTIA==9]=2
table(is.na(Adult20Health$DEMENTIA))


Adult20Health = subset(Adult20Health, HYPERTENSION!=2)
Adult20Health = subset(Adult20Health, HIGH_CHOLESTEROL!=2)
Adult20Health = subset(Adult20Health, HEART_DISEASE!=2)
Adult20Health = subset(Adult20Health, ANGINA!=2)
Adult20Health = subset(Adult20Health, HEART_ATTACK!=2)
Adult20Health = subset(Adult20Health, STROKE!=2)
Adult20Health = subset(Adult20Health, CANCER!=2)
Adult20Health = subset(Adult20Health, DIABETES!=2)
Adult20Health = subset(Adult20Health, HOSPITAL_OVERNIGHT!=2)
Adult20Health = subset(Adult20Health, THERAPY_NOT_MENTAL!=2)
Adult20Health = subset(Adult20Health, LUNG_DISEASE!=2)
Adult20Health = subset(Adult20Health, ARTHRITIS!=2)
Adult20Health = subset(Adult20Health, DEMENTIA!=2)
table(Adult20Health$HYPERTENSION)
table(Adult20Health$STROKE)





#Data Visualization
#Histograms of Dependent Variables
hist(Adult20$ANXLEVEL_A)
hist(Adult20$DEPLEVEL_A)
hist(Adult20Actionable$ANXLEVEL_A)
hist(Adult20Actionable$DEPLEVEL_A)
hist(Adult20Health$ANXLEVEL_A)
hist(Adult20Health$DEPLEVEL_A)


#Correlations
#All variables are factors or binary - so we cannot do any correlation charts.
#We will be using ordinal factors for some variables though, and treating
#them as numerical in the model.

#We can still check for multicollinearity using the vif function and taking out
#variables showing mulicollinearity.
cor(Adult20Actionable$ANXLEVEL_A, Adult20Actionable$DEPLEVEL_A)
cor(Adult20Health$ANXLEVEL_A, Adult20Health$DEPLEVEL_A)
#Correlation between variables is relatively high, so we will not be able to use
#one DV as a predictor for the other.

#Boxplots
boxplot(Adult20Actionable$ANXLEVEL_A~Adult20Actionable$nicotine)
boxplot(Adult20Actionable$ANXLEVEL_A~Adult20Actionable$walk)
boxplot(Adult20Actionable$ANXLEVEL_A~Adult20Actionable$sleep)
boxplot(Adult20Actionable$ANXLEVEL_A~Adult20Actionable$Exercise)
boxplot(Adult20Actionable$ANXLEVEL_A~Adult20Actionable$weightloss)
boxplot(Adult20Actionable$ANXLEVEL_A~Adult20Actionable$alcohol)
boxplot(Adult20Actionable$ANXLEVEL_A~Adult20Actionable$bmi)
boxplot(Adult20Actionable$ANXLEVEL_A~Adult20Actionable$FoodSecurity)

boxplot(Adult20Actionable$DEPLEVEL_A~Adult20Actionable$nicotine)
boxplot(Adult20Actionable$DEPLEVEL_A~Adult20Actionable$walk)
boxplot(Adult20Actionable$DEPLEVEL_A~Adult20Actionable$sleep)
boxplot(Adult20Actionable$DEPLEVEL_A~Adult20Actionable$Exercise)
boxplot(Adult20Actionable$DEPLEVEL_A~Adult20Actionable$weightloss)
boxplot(Adult20Actionable$DEPLEVEL_A~Adult20Actionable$alcohol)
boxplot(Adult20Actionable$DEPLEVEL_A~Adult20Actionable$bmi)
boxplot(Adult20Actionable$DEPLEVEL_A~Adult20Actionable$FoodSecurity)



boxplot(Adult20Health$ANXLEVEL_A~Adult20Health$HYPERTENSION)
boxplot(Adult20Health$ANXLEVEL_A~Adult20Health$HIGH_CHOLESTEROL)
boxplot(Adult20Health$ANXLEVEL_A~Adult20Health$HEART_DISEASE)
boxplot(Adult20Health$ANXLEVEL_A~Adult20Health$ANGINA)
boxplot(Adult20Health$ANXLEVEL_A~Adult20Health$HEART_ATTACK)
boxplot(Adult20Health$ANXLEVEL_A~Adult20Health$STROKE)
boxplot(Adult20Health$ANXLEVEL_A~Adult20Health$ASTHMA)
boxplot(Adult20Health$ANXLEVEL_A~Adult20Health$CANCER)
boxplot(Adult20Health$ANXLEVEL_A~Adult20Health$DIABETES)
boxplot(Adult20Health$ANXLEVEL_A~Adult20Health$LUNG_DISEASE)
boxplot(Adult20Health$ANXLEVEL_A~Adult20Health$ARTHRITIS)
boxplot(Adult20Health$ANXLEVEL_A~Adult20Health$DEMENTIA)
boxplot(Adult20Health$ANXLEVEL_A~Adult20Health$HOSPITAL_OVERNIGHT)
boxplot(Adult20Health$ANXLEVEL_A~Adult20Health$THERAPY_NOT_MENTAL)

boxplot(Adult20Health$DEPLEVEL_A~Adult20Health$HYPERTENSION)
boxplot(Adult20Health$DEPLEVEL_A~Adult20Health$HIGH_CHOLESTEROL)
boxplot(Adult20Health$DEPLEVEL_A~Adult20Health$HEART_DISEASE)
boxplot(Adult20Health$DEPLEVEL_A~Adult20Health$ANGINA)
boxplot(Adult20Health$DEPLEVEL_A~Adult20Health$HEART_ATTACK)
boxplot(Adult20Health$DEPLEVEL_A~Adult20Health$STROKE)
boxplot(Adult20Health$DEPLEVEL_A~Adult20Health$ASTHMA)
boxplot(Adult20Health$DEPLEVEL_A~Adult20Health$CANCER)
boxplot(Adult20Health$DEPLEVEL_A~Adult20Health$DIABETES)
boxplot(Adult20Health$DEPLEVEL_A~Adult20Health$LUNG_DISEASE)
boxplot(Adult20Health$DEPLEVEL_A~Adult20Health$ARTHRITIS)
boxplot(Adult20Health$DEPLEVEL_A~Adult20Health$DEMENTIA)
boxplot(Adult20Health$DEPLEVEL_A~Adult20Health$HOSPITAL_OVERNIGHT)
boxplot(Adult20Health$DEPLEVEL_A~Adult20Health$THERAPY_NOT_MENTAL)

#Regression models. Since we are treating our ordininal variable as continuous, 
#we are doing OLS regression here.
m_AnxietyAction = lm(ANXLEVEL_A~relevel(nicotine, ref="Never")+walk+sleep+relevel(Exercise, ref="Not Active")+weightloss+relevel(alcohol, ref="Never")+bmi+FoodSecurity, data=Adult20Actionable)
summary(m_AnxietyAction)

m_DepressionAction = lm(DEPLEVEL_A~relevel(nicotine, ref="Never")+walk+sleep+relevel(Exercise, ref="Not Active")+weightloss+relevel(alcohol, ref="Never")+bmi+FoodSecurity, data=Adult20Actionable)
summary(m_DepressionAction)

m_AnxietyHealth = lm(ANXLEVEL_A~HYPERTENSION+HIGH_CHOLESTEROL+HEART_DISEASE+ANGINA+HEART_ATTACK+STROKE+ASTHMA+CANCER+DIABETES+LUNG_DISEASE+ARTHRITIS+DEMENTIA+HOSPITAL_OVERNIGHT+THERAPY_NOT_MENTAL, data=Adult20Health)
summary(m_AnxietyHealth)

m_DepressionHealth = lm(DEPLEVEL_A~HYPERTENSION+HIGH_CHOLESTEROL+HEART_DISEASE+ANGINA+HEART_ATTACK+STROKE+ASTHMA+CANCER+DIABETES+LUNG_DISEASE+ARTHRITIS+DEMENTIA+HOSPITAL_OVERNIGHT+THERAPY_NOT_MENTAL, data=Adult20Health)
summary(m_DepressionHealth)

#We are going to split the stargazer into two, since there is a total split between
#variables analyzed.
library(stargazer)
stargazer(m_AnxietyAction, m_DepressionAction, type="text", single.row=TRUE)
stargazer(m_AnxietyHealth, m_DepressionHealth, type="text", single.row=TRUE)

#Testing assumptions
plot(m_AnxietyAction, 1)
plot(m_DepressionAction, 1)
plot(m_AnxietyHealth, 1)
plot(m_DepressionHealth, 1)

plot(m_AnxietyAction, 2)
plot(m_DepressionAction, 2)
plot(m_AnxietyHealth, 2)
plot(m_DepressionHealth, 2)

library(lmtest) 
dwtest(m_AnxietyAction)
dwtest(m_DepressionAction)
dwtest(m_AnxietyHealth)
dwtest(m_DepressionHealth)

library(car)
vif(m_AnxietyAction)
vif(m_DepressionAction)
vif(m_AnxietyHealth)
vif(m_DepressionHealth)
