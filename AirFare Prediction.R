#############INITIALIZE DATA###################################
mydata <- read.csv("AirfaresData.csv")
names(mydata)
Catdata <- mydata[ ,c(5,6,7,8,9,10,11,12,13,14,15,16,17,18)]
names(Catdata)
View(Catdata)

numericlm <- lm(FARE ~ COUPON + NEW + HI + S_INCOME + E_INCOME + S_POP + E_POP + DISTANCE + PAX, data = mynumeric)
#####CORRELATION##########################################
mynumeric <- mydata[,c(5,6,9,10,11,12,13,16,17,18)]
mynumeric
cor(mynumeric)
plot(mynumeric$COUPON,mynumeric$FARE, xlab = 'COUPON',ylab = 'FARE')
plot(mynumeric$FARE,mynumeric$NEW) #Not proper
plot(mynumeric$HI,mynumeric$FARE,xlab = 'HI',ylab = 'FARE')
plot(mynumeric$S_INCOME,mynumeric$FARE) #Not proper
plot(mynumeric$E_INCOME,mynumeric$FARE) #Not proper
plot(mynumeric$S_POP,mynumeric$FARE,xlab = 'S_POPULATION',ylab = 'FARE')    #Not proper
plot(mynumeric$E_POP,mynumeric$FARE,xlab = 'E_POPULATION',ylab = 'FARE')    #Not proper
plot(mynumeric$DISTANCE,mynumeric$FARE,xlab = 'Distance',ylab = 'FARE')
plot(mynumeric$PAX,mynumeric$FARE,xlab = 'PAX', ylab = 'FARE')

###########CATEGORICAL PREDICTOR#############################

V <- aggregate(FARE ~ VACATION, data = mydata, FUN = mean)
V
S <- aggregate(FARE ~ SW, data = Catdata, FUN = mean)
S
SL <- aggregate(FARE ~ SLOT, data = Catdata, FUN = mean)
SL
GA <- aggregate(FARE ~ GATE, data = Catdata, FUN = mean)
GA

######################TRAINING#################################
set.seed(12345)
train <- sample(2,row,replace = TRUE, prob = c(0.6,0.4))
trainingindex <- mydata[train ==1,]
validaindex <- mydata[train ==2,]
result <-lm(FARE ~ DISTANCE + SW, trainingindex)
summary(result)
par(mfrow=c(2,2))
plot(result)
summary(result)$r.squared 
BIC(result)
AIC(result)
###############################################################

######BACKWARD REGRESSION #####################################

back <- step(lm(FARE ~ COUPON + NEW + VACATION +SW + HI + S_INCOME + E_INCOME + S_POP + E_POP +SLOT + GATE+ DISTANCE + PAX,data = trainingindex),direction = "backward")
summary(back)
summary(back)$r.squared
BIC(back)
AIC(back)



############BEST MODEL#########################################

AIC(result)
AIC(back)

############PREDICT - PART G###################################
coeff <- coefficients(back)
coeff
partg <- coeff[1] + coeff[2]*VACATION + coeff[3]*SW + coeff[4]*HI + 
         coeff[5]*S_INCOME + coeff[6]*S_POP + coeff[7]*E_POP + 
         coeff[8]*SLOT + coeff[9]*GATE + coeff[10]*DISTANCE + 
         coeff[11]*PAX

partg <- coeff[1] + coeff[2]*0 + coeff[3]*0 + coeff[4]*4442.141 + 
         coeff[5]*28760 + coeff[6]*4557004 + coeff[7]*3195503 + 
         coeff[8]*1+ coeff[9]*1 + coeff[10]*1976 + coeff[11]*12782
partg

partgsw <- coeff[1] + coeff[2]*0 + coeff[3]*1 + coeff[4]*4442 + 
           coeff[5]*28760 + coeff[6]*4557004 + coeff[7]*3195503 + 
           coeff[8]*1+ coeff[9]*1 + coeff[10]*1976 + coeff[11]*12782
partgsw

########################PART -I ###########################################

parti <- lm(FARE ~ COUPON + NEW + VACATION + SW + HI + S_INCOME + E_INCOME + S_POP + E_POP + SLOT + GATE + DISTANCE, data = trainingindex)
summary(parti)
AIC(parti)
