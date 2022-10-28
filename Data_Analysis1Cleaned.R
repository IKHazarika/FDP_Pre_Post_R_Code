#We import the relevant libraries required for CFA and SEM
library(psych)
library(GPArotation)
library(REdaS)

library(lavaan)
library(semPlot)
library(semTools)
library(semTools)
library(dplyr)
library(lavaanPlot)

#We use the Final_Compiled.xls dataset which has been cleaned by Sourabh

#We mark Q17 in the questionnaire as problematic and run the analysis with and without it separately
#Con4Neg is problematic, we dont include that in dataf2
dataf <- Final_Compiled[, 11:28]
dataf2 <- dataf[, -17]

#We calculate the Cronbach's alpha score for all four of our latent variables
alpha(dataf[, 1:5]) #Satisfaction
alpha(dataf[, c(6:9, 18)]) #Relevance
alpha(dataf[, 10:13]) #Attitude
alpha(dataf[, 14:16]) #Confidence

#We define the first measurement model
model1 <- '
satisfaction =~ Sat1 + Sat2 + Sat3 + Sat4
relevance =~ Rel1 + Rel2 + Rel3 + Rel4 + Rel5
attitude =~ Att1 + Att2 + Att3 + Att4
confidence =~ Con1 + Con2 + Con3'

#We see the model fit statistics and find underfit
model1.fit <- cfa(model1, data = dataf2)

summary(model1.fit, standardized = TRUE)
fitMeasures(model1.fit)

#We use the modindices command to trace out which relationship is leading to underfit
#We also generate a graph of the path model for clearer understanding
modindices(model1.fit, minimum.value = 10, sort = TRUE)

lavaanPlot(model = model1.fit, node_options = list(shape = "box", fontname = "Helveltica"), edge_options = list(color = "grey"), coef = TRUE, stand = TRUE, covs = TRUE)

#We make Gaskin's table
#No R library is available for this purpose
#While it is possible to define functions in R for this, Gaskin has already developed an Excel VBA tool for this, we use that directly instead

reliability(model1.fit)

#                CR  AVE  MSV
# satisfaction 0.96 0.85 0.22      
# relevance    0.91 0.67 0.51
# attitude     0.89 0.67 0.46
# confidence   0.93 0.81 0.51

#Covariances:
#  Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
#satisfaction ~~                                                       
#  relevance         0.457    0.072    6.366    0.000    0.445    0.445
#  attitude          0.424    0.065    6.484    0.000    0.457    0.457
#  confidence        0.472    0.074    6.349    0.000    0.438    0.438
#relevance ~~                                                          
#  attitude          0.631    0.063   10.040    0.000    0.951    0.951
#  confidence        0.712    0.071   10.059    0.000    0.925    0.925
#  attitude ~~                                                           
#  confidence        0.677    0.066   10.317    0.000    0.974    0.974

#We add our non-latent (observable) variables to the dataframe for further analysis
dataf2$gain <- Final_Compiled$Posttest - Final_Compiled$Pretest
dataf2$attendance <- Final_Compiled$Attendance

#We now combine the measurement and path models to get the full SEM
model2 <- '
satisfaction =~ Sat1 + Sat2 + Sat3 + Sat4
relevance =~ Rel1 + Rel2 + Rel3 + Rel4 + Rel5
attitude =~ Att1 + Att2 + Att3 + Att4
confidence =~ Con1 + Con2 + Con3

attitude ~ satisfaction + attendance + gain
attitude ~~ confidence
attitude ~~ satisfaction
confidence ~~ gain + attendance
confidence ~~ satisfaction
satisfaction ~ attendance + gain + relevance
attendance ~ relevance
gain ~ attendance'

#We test the fit of the full SEM
model2.fit <- sem(model2, data = dataf2, std.lv = TRUE)

summary(model2.fit, fit = TRUE, standardized = TRUE, rsquare = TRUE)

fitmeasures(model2.fit)

reliability(model2.fit)

#The model is satisfactory, so we generate a graph to report our analysis

lavaanPlot(model = model2.fit, node_options = list(shape = "box", fontname = "Helveltica"), edge_options = list(color = "grey"), coef = TRUE, stand = TRUE, covs = TRUE)