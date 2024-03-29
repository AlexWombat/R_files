#install.packages("ggplot2")
#install.packages("foreach")
install.packages("xlsx")

source("C:\\Users\\alexa\\Documents\\År 3\\Regression analysis\\Project 2\\multiplot.R")

library(ggplot2)
library(foreach)

######################### Section 1: Read data #########################

# This is where your GLM data is read form tractors.csv into a table in R 
# Note that the folder in which you have tractors.csv must be set as the working directory 
###### You do not need to change anything in this section. The data will be sotred in a table named glmdata

glmdata <- read.table("C:\\Users\\alexa\\Documents\\År 3\\Regression analysis\\Project 2\\Tractors.csv", header=TRUE, sep=";", dec="," )

######################### Section 2: Create groups & aggregate data #########################

# Now you need to modify your data so that you can perform a GLM analysis 

# First, any continuous variable needs to be grouped into discrete groups 
# The code below groups the variable weight, from you table glmdata, into six groups, and stores this in a new column, weight_group 
# It also groups the variable VehicleAge into four groups
###### This is only an example. You need to create your own groups, with breaks that suit your data
###### You might also want to group other variables from glmdata, in a similar manner

glmdata$weight_group <- cut(glmdata$Weight, 
                       breaks = c(-Inf, 500, 1000, 2000, 3500, 5000, Inf), 
                       labels = c("01_<500kg", "02_500-999kg", "03_1000-1999kg", "04_2000-3499kg", "05_3500-4999kg", "06_>=5000g"), 
                       right = FALSE)

#glmdata$weight_group <- cut(glmdata$Weight, 
#                            breaks = c(-Inf, 1000, 2000, 3000, 4000, 5000, Inf), 
#                            labels = c("01_<1000kg", "02_1000-1999kg", "03_2000-2999kg", "04_3000-3999kg", "05_4000-4999kg", "06_>=5000kg"), 
#                            right = FALSE)


glmdata$age_group <- cut(glmdata$VehicleAge, 
                            breaks = c(-Inf, 3, 6, 9, 14, 22, Inf), 
                            labels = c("01_<3years", "02_3-5years", "03_6-8years", "04_9-13years", "05_14-21years", "06_>=22"), 
                            right = FALSE)

#glmdata$riskyear_group <- cut(glmdata$RiskYear,
#                          breaks = c(-Inf, 2008, 2010, 2012, 2014, 2016, Inf),
#                          labels = c('01_<2008', '02_2008-2009', '03_2010-2011', '04_2012-2013', '05_2014-2015', '06_>2016'),
#                          right = FALSE)

# Secondly, we want to aggregate the data.
# That is, instead of having one row per tractor, we want one row for each existing combination of variables 
# This code aggregates columns 6-8 of glmdata, by three variables: weight_group, Climate, and ActivityCode 
# Tha aggregated data is stored in a new table, glmdata2 
##### You need to consider if there are any other variables you want to aggregate by, and modify the code accordingly 

glmdata2 <- aggregate(glmdata[,6:8],by=list(weight_group = glmdata$weight_group, 
                                            Climate = glmdata$Climate,
                                            ActivityCode = glmdata$ActivityCode,
                                            #riskyear_group = glmdata$riskyear_group,
                                            age_group = glmdata$age_group),FUN=sum, na.rm=TRUE) 

# We then do some preparation for the output the GLM function will give.
# This piece of code creates a new table, glmdata3, with a row per variable and group, and with data on the total duration corresponding to this group.
##### You need ot modify the code to take into account any changes in variables you're using 

glmdata3 <-
  data.frame(rating.factor =
               c(rep("Weight", nlevels(glmdata2$weight_group)),
                 rep("Climate", nlevels(glmdata2$Climate)),
                 rep("ActivityCode", nlevels(glmdata2$ActivityCode)),
                 rep("Age", nlevels(glmdata2$age_group))),    #rep('RiskYear', nlevels(glmdata2$riskyear_group))),
             class =
               c(levels(glmdata2$weight_group),
                 levels(glmdata2$Climate),
                 levels(glmdata2$ActivityCode),
                 levels(glmdata2$age_group)),                     #levels(glmdata2$riskyear_group))
             stringsAsFactors = FALSE)

new.cols <-
  foreach (rating.factor = c("weight_group", "Climate", "ActivityCode", "age_group"),   #, 'riskyear_group'
           .combine = rbind) %do%
           {
             nclaims <- tapply(glmdata2$NoOfClaims, glmdata2[[rating.factor]], sum)
             sums <- tapply(glmdata2$Duration, glmdata2[[rating.factor]], sum)
             n.levels <- nlevels(glmdata2[[rating.factor]])
             contrasts(glmdata2[[rating.factor]]) <-
               contr.treatment(n.levels)[rank(-sums, ties.method = "first"), ]
             data.frame(duration = sums, n.claims = nclaims)
           }
glmdata3 <- cbind(glmdata3, new.cols)
rm(new.cols)

######################### Section 3: GLM analysis #########################

# Now we get to the fun part - the GLM analysis. It is performed using R's built in GLM function 

# First, we model the claims frequency. 
# The first part of this performs a GLM analysis, with glmdata2 as the data source modelling NoOfClaims, by the Duration. It looks at three variables: weight_group, Climate, and ActivityCode.
##### This is where you can modify the model by adding or removing variables 

model.frequency <-
  glm(NoOfClaims ~ weight_group + Climate + ActivityCode + age_group + offset(log(Duration)), # + riskyear_group
      data = glmdata2, family = poisson)

# Then we save the coefficients resulting from the GLM analysis in an array
##### You should not need to modify this part of the code

rels <- coef(model.frequency)
rels <- exp(rels[1] + rels[-1])/exp(rels[1])

# Finally, we attach the coefficients to the already prepared table glmdata3, in a column named rels.frequency
# There is no good way of doing this automatically, so we need to do some manual tricks
# This code creates a vector with 6 positions consisting of the integer 1, and then positions number 1-5 in the rels array.
# Then it attaches this to rows 1-6 of glmdata3, sorted from highest to lowest duration, since the GLM data is on this form.
# In other words, the code takes the GLM coeffisients for the six weight groups and saves those in glmdata3, in the rows corresponding to those groups.
# After that, it does the same thing for the rest of the GLM coefficients, belonging to climate and activity code vairables.
##### You need to modify this code to suit your set of variables and groups, to make sure each GLM coefficient is saved in the correct place.

##### You need to modify this code
variableLevels <- c(nlevels(glmdata2[["weight_group"]]),
                 nlevels(glmdata2[["Climate"]]),
                 nlevels(glmdata2[["ActivityCode"]]),
                 nlevels(glmdata2[["age_group"]]))

#You do not need to modify this part
cs <- cumsum(variableLevels)
cs_rels <- cs
for(i in 1:length(variableLevels)){
  cs_rels[i] <- cs[i]-i
}

# The following code needs to be used at two different places so we put it in a function.
##### This part needs to be modified if you change which variables are included in the model, but not if you change the groups inside a variable
attachRels <- function(rels_vec, vector, cs, cs_rels) {
  c(c(1, rels_vec[ 1 : cs_rels[1] ])[rank(-vector[ 1 : cs[1] ], ties.method = "first")],
    c(1, rels_vec[ (cs_rels[1]+1) : cs_rels[2] ])[rank(-vector[ (cs[1]+1) : cs[2] ], ties.method = "first")],
    c(1, rels_vec[ (cs_rels[2]+1) : cs_rels[3] ])[rank(-vector[ (cs[2]+1) : cs[3] ], ties.method = "first")],
    c(1, rels_vec[ (cs_rels[3]+1) : cs_rels[4] ])[rank(-vector[ (cs[3]+1) : cs[4] ], ties.method = "first")])
}

# Use the function created above, you do not need to modify this part
glmdata3$rels.frequency <- attachRels(rels, glmdata3$duration, cs, cs_rels)

# We then do the same thing again, now modelling severity instead of claim frequency.
# That means that, in this part, we want to look at the average claim. So first, we calculate the average claim for each row in glmdata2
##### You should not need to change anything in this piece of code.

glmdata2$avgclaim=glmdata2$ClaimCost/glmdata2$NoOfClaims

# Then we do the same thing as we did when modelling claims frequency, but we look at average claim;
# A GLM analysis is run, the coefficients stored, and saved in a new column, named rels.severity, glmdata3
##### You need to modify this part of the code in the same way as you did for the GLM model for frequency.  Add or remove variables
##### Remember that, according to the project instructions, you need to use the same variables for the severity as for the frequency.

model.severity <-
  glm(avgclaim ~ weight_group + Climate + ActivityCode + age_group ,
      data = glmdata2[glmdata2$avgclaim>0,], family = Gamma("log"), weight=NoOfClaims)

# You do not need to change this part
rels <- coef(model.severity)
rels <- exp( rels[1] + rels[-1] ) / exp( rels[1] )
glmdata3$rels.severity <- attachRels(rels, glmdata3$duration, cs, cs_rels)

# Finally, the final risk factor is calculated, as the product of the frequency and severity factors. 
##### You should not have to modify this coed.
##### Congratulations! You now have a model for the risk!
glmdata3$rels.risk <- with(glmdata3, rels.frequency*rels.severity)

######################### Section 4: Plotting #########################

# In this section, the results from the GLM are plotted.

# First, long variable names need to be cut, to fit into the plots.
# This row of code cuts away everything except for the first letter for variable names belonging to activity codes.
##### If you have long variable names, modify here to cut them.
glmdata3[glmdata3$rating.factor == "ActivityCode",2] <- substr(glmdata3$class,1,2)[10:20]  


# Then the results are plotted. This code plots the GLM factors for frequency, severity, and total risk, for the three variables Weight, Climate, and Activity code.
##### If you have changed what variables are included in your model, add, remove, or modify sections of this code to plot them. 
##### This is also where you can make changes to change the look of your plots, if you would like to.

p1 <- ggplot(subset(glmdata3, rating.factor=="Weight"), aes(x=class, y=rels.frequency)) + 
      geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Weight: frequency factors") +
      geom_text(aes(label=paste(round(rels.frequency,2))), nudge_y=1) +theme(axis.text.x = element_text(angle = 30, hjust = 1))

p2 <- ggplot(subset(glmdata3, rating.factor=="Weight"), aes(x=class, y=rels.severity)) + 
      geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Weight: severity factors") +
      geom_text(aes(label=paste(round(rels.severity,2))), nudge_y=0.5)+theme(axis.text.x = element_text(angle = 30, hjust = 1))

p3 <- ggplot(subset(glmdata3, rating.factor=="Weight"), aes(x=class, y=rels.risk)) + 
      geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Weight: risk factors") +
      geom_text(aes(label=paste(round(rels.risk,2))), nudge_y=1.6)+theme(axis.text.x = element_text(angle = 30, hjust = 1))

p4 <- ggplot(subset(glmdata3, rating.factor=="Climate"), aes(x=class, y=rels.frequency)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Climate: frequency factors") +
  geom_text(aes(label=paste(round(rels.frequency,2))), nudge_y=0.05)

p5 <- ggplot(subset(glmdata3, rating.factor=="Climate"), aes(x=class, y=rels.severity)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Climate: severity factors") +
  geom_text(aes(label=paste(round(rels.severity,2))), nudge_y=0.1)

p6 <- ggplot(subset(glmdata3, rating.factor=="Climate"), aes(x=class, y=rels.risk)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Climate: risk factors") +
  geom_text(aes(label=paste(round(rels.risk,2))), nudge_y=0.1)

p7 <- ggplot(subset(glmdata3, rating.factor=="ActivityCode"), aes(x=class, y=rels.frequency)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("ActivityCode: frequency factors") +
  geom_text(aes(label=paste(round(rels.frequency,2))), nudge_y=0.5) 

p8 <- ggplot(subset(glmdata3, rating.factor=="ActivityCode"), aes(x=class, y=rels.severity)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("ActivityCode: severity factors") +
  geom_text(aes(label=paste(round(rels.severity,2))), nudge_y=0.5)

p9 <- ggplot(subset(glmdata3, rating.factor=="ActivityCode"), aes(x=class, y=rels.risk)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("ActivityCode: risk factors") +
  geom_text(aes(label=paste(round(rels.risk,2))), nudge_y=0.5)

p10 <- ggplot(subset(glmdata3, rating.factor=="Age"), aes(x=class, y=rels.frequency)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Age: frequency factors") +
  geom_text(aes(label=paste(round(rels.frequency,2))), nudge_y=0.5) 

p11 <- ggplot(subset(glmdata3, rating.factor=="Age"), aes(x=class, y=rels.severity)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Age: severity factors") +
  geom_text(aes(label=paste(round(rels.severity,2))), nudge_y=0.5)

p12 <- ggplot(subset(glmdata3, rating.factor=="Age"), aes(x=class, y=rels.risk)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Age: risk factors") +
  geom_text(aes(label=paste(round(rels.risk,2))), nudge_y=0.5)



multiplot(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, cols=4)



######################### Section 5: Export factors to Excel #########################

#As a last step, the risk factors are exported to excel. 
# The dopcument will be saved in the folder set as your working directory.

write.csv(glmdata3, "C:\\Users\\alexa\\Documents\\År 3\\Regression analysis\\Project 2\\glmfactors.csv")


active_ins = subset(glmdata, RiskYear == '2016')

exp_claim_cost <- ifelse(active_ins$Duration != 0, active_ins$ClaimCost/active_ins$Duration, 0)
tot_exp_claim_cost <- sum(exp_claim_cost)

#exp_claim_cost_prev  <- sum(active_ins$ClaimCost*(2-active_ins$Duration))
total_premium <- tot_exp_claim_cost/0.9


total_risk_factor <- glmdata3[as.character(active_ins$Climate), 'rels.risk'] * glmdata3[as.character(active_ins$ActivityCode), 'rels.risk'] * glmdata3[as.character(active_ins$weight_group), 'rels.risk'] * glmdata3[as.character(active_ins$age_group), 'rels.risk']
# sum(total_risk_factor_per_insurance * gamma_null) = total_premium
gamma_null <- total_premium/sum(total_risk_factor)
