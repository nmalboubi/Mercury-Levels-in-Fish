
**Introduction:**

Something smells a little bit fishy! The North Carolina Department of Health and Human Services has monitored the levels of mercury in largemouth bass in the local waterways. They have sampled fish from two different rivers, of which we will call River0 and River1. In addition, these rivers were further divided up into 16 locations, known as "Stations", and fish from each station were sampled and analyzed. In this study, we will model a relationship between mercury found in the fish given their station location, their length, weight, and whether or not they are deemed safe to eat.  



**Preliminary Analysis:**

I first looked at the data to see if I could spot any collinearity, or if I should transform any of the data.

**Plot 1.1A**

```{r, echo=FALSE}


#observing and cleaning data
load ("C:/Users/Owner/Downloads/mercury.rda")
data = mercury
modNames = c("mod0", "mod1", "mod2", "mod3", "mod4", "mod5")
pairs(data)
```

There appears to be high correlation between length and weight, and thus I will remove their respective interaction term from all the models.

I also looked at histograms of the weight, length and mercury levels of the fish, in order to see if transformations would be necessary.

**Plot 1.1B**

```{r, echo=FALSE}


#Are log transformations necessary to normalize data
par(mfrow=c(3,2))

hist(data$weight, main = "Histogram of Weight of Bass", xlab="Weight (gg)")

hist(log(data$weight), main = "Histogram of Log of Weight", xlab= "Log of Weight (g)")



hist(data$length, main = "Histogram of Length of Bass", xlab = "Length (cm)")

hist(log(data$length), main = "Histogram of Log of Length of Bass", xlab = "Log of Length (cm)")


hist(data$mercury, main ="Histogram of Mercury found in Bass", xlab = "Mercury (mg)")
hist(log(data$mercury), main ="Histogram of Log of Mercury", xlab = "Log of Mercury (mg)")

```

In order to minimize the increasing variance, I transformed that data with log functions, as the distributions appeared to become more normal by doing so. And since I was curious if location was correlated with mercury levels in the fish, I also introduced an indicator variable, data$safe, that described whether the fish sampled was toxic (containing more than 2 mg of mercury). 

```{r, echo=FALSE}
data$logweight = log(data$weight)

data$loglength = log(data$length)

data$logmerc = log(data$mercury)

#Is the fish safe to eat
data$safe = sapply(data$mercury, function(x){
  if (x<2){1} else{0}
  
  })


```     
     

The next few plots show the relationships between location of the bass and the levels of mercury found.

Below is a graph (**Plot 1.2a**) of the mercury Vs weight (both are transformed) dependent on River0 and River1

**Plot 1.2a**

```{r, echo=FALSE}

coplot(logmerc~logweight | factor(river), data=data)
```

There appear to be more fish in River 1 that have higher levels of mercury. In addition, in River0, there appears to be no relationship between mercury and weight, whereas in River1, there looks like there is a clear linear relationship between weight and mercury levels. In addition, it appears that smaller fish are less likely to have dangerous levels of mercury in them.



Below is a graph (**Plot 1.2b**) of mercury on weight (both are transformed) given the station of the fish. 

**Plot 1.2b**


```{r,echo=FALSE}

coplot(logmerc~logweight| factor(station), data=data)     

row_Names = c("Station 0 ", "Station 1", "Station 2", "Station 3", "Station 4", "Station 5", "Station 6", "Station 7", "Station 8", "Station 9", "Station 10", "Station 11", "Station 12", "Station 13", "Station 14", "Station 15" )


```{r, echo=FALSE}
fishamt = vector()
for (i in 1:16){
fishamt[i] =  length(which (data$station==i-1))}


fishsafe = vector()
for (i in 1:16){
  fishsafe[i] = sum(data$safe[which(data$station==i-1)])}

     
   

fishsafeper = fishsafe/fishamt

loc = list()
for (i in 1:16)
loc[[i]] = rep(fishsafeper[i], length(which(data$station==i-1)))

loc=unlist(loc)

data$undwe = sapply(data$weight, function (x){
  if (x<2000){1}else{0}
})

  
#data$safe = sapply(data$mercury, function(x){
 # if (x<2){1} else{0}
  
  

loc2 = vector()
 
for(i in 1:171){
  loc2[i] = if (data$station[i]==1) {1} else {0
    if (data$station[i]==4){1} else {
      if (data$station[i]==11) {1} else {
        if (data$station[i]==12) {1} else {0}}}}}
data$loc2 = loc

```
In many of the stations, there is a positive linear relationship between mercury and weight. Stations 1-3, and 13 show no clear relationship between mercury and weight. However, due to small sample sizes from these locations, removing any particular station from the data, can skew results dramatically, and therefore I have decided to keep all stations.



**Table 1.1** in the appendix clearly shows the safety of certain fish in certain locations of the river. For example, only 45% of the fish in station 15 sample have less than 2mg of mercury. Also, from the third co-plot below, it can be seen that a smaller proportion of lighter fish are deemed unsafe, where as the proportion increases as the fish weight increases.

**1** = Fish have less than 2mg mercury

**0** = Fish have more than 2mg mercury (dangerous)

**Plot 1.3**


```{r, echo=FALSE, fig.height=5}
par(mar=c(0,0,0,0)) 

coplot(mercury~factor(station)|factor(safe), data = data)     
  
```     


The above plot displays the relationship between stations and whether the fish are safe.The factor of 1, represents that the fish have less than 2 milligrams of mercury, and the factor of 0 represents that the fish have more than 2 milligrams of mercury. From this plot, it appears that there are high levels of toxic fish in stations 15 and 10 that would not be safe for public consumption. 


**Analysis:**

Our first model is the full model, as I regress the log of mercury on the log of weight, the log of length, river location, river type, and the interactions among them. The subsequent models would remove certain terms (either parameters themselves, or their interactions). Again, I did not include the interaction between length and weight because the high collinearity between the two parameters. **Table 1.2** in the appendix describes the different models.



```{r, echo=FALSE, include=FALSE}
mod0 = lm(data$logmerc ~ data$logweight + data$loglength + factor(data$river) + as.factor (data$station)  +  data$logweight:factor(data$river) + data$logweight:factor(data$station) +data$loglength:factor(data$river) +data$loglength:factor(data$station)) 

plot(data$loglength~data$station)
summary(mod0)
#high collinearity

plot(resid(mod0)~fitted(mod0))
#looks pretty good.


plot(residuals(mod0)~data$station, xlab = "Station Number", ylab = "Residuals", main = "Model: Mercury in Fish ~ logweight")


#station 10 and 4 appear to have a large residuals. Therefore, I need to look at potential outliers that might be skewing the observations.

#high R
     
     
```
There is high correlation between the rivers, and station numbers. Thus, I removed river type from the data set. I chose river over station, because the residual sum of squares increased dramatically upon the removal of station from the data set. The residual sum of squares slightly increased when removing river.

```{r, echo=FALSE, include=FALSE}
     
     
#dropped river     
mod1 = lm(data$logmerc ~ data$logweight + data$loglength +  as.factor (data$station)   + data$logweight:factor(data$station) + data$loglength:factor(data$station))

#dropped station
mod1b = lm(data$logmerc ~ data$logweight + data$loglength + factor(data$river)+  data$logweight:factor(data$river) + data$loglength:factor(data$river))

summary(mod1b)

summary(mod1)
  
anova(mod0, mod1)
anova(mod0, mod1b)     

```

```{r, echo=FALSE}
d = matrix(c(anova(mod0,mod1)$RSS,anova(mod0,mod1b)$RSS[2]))

rownames(d) = c("With River and Station", "With Station/Remove River", "With River/Remove Station ")
colnames(d) = "RSS"

d=as.table(d)
d
```
The table above displays the residual sum of squares for 3 models. As one can see, the RSS remains 15.677 when dropping river from the data set, whereas it jumps to 34.845, when dropping river. Therefore, river will be removed from the data set. The remaining models test the significance of the interaction terms.


```{r, echo=FALSE, include=FALSE}
summary(mod1)
plot(mod1$resid~mod1$fitted)

lm(data$logmerc ~ data$logweight + data$loglength +  as.factor (data$station)   + data$logweight:factor(data$station) + data$loglength:factor(data$station))
     


#kept mod1, removed interaction of loglength and station    
mod2 =lm(data$logmerc ~ data$logweight + data$loglength +  as.factor (data$station)   + data$logweight:factor(data$station))


anova(mod1, mod2)

summary(mod2)
plot(mod2$resid~mod2$fitted)

#kept mod2 removed interaction of logweight and station    
mod3 = lm(data$logmerc ~ data$logweight + data$loglength +  as.factor (data$station))


anova(mod2, mod3)    
     
plot(mod3$resid~mod3$fitted)

summary(mod3)

#kept mod1, removed both interactions
mod4 = lm(data$logmerc ~ data$logweight+ data$loglength)

summary(mod4)

anova(mod3,mod4)     
     
     
#removed all interaction and weight   
mod5 = lm(data$logmerc ~data$logweight)

anova(mod4, mod5)     
summary(mod5)


par(mfrow=c(2,2))
plot(rstudent(mod2)~mod2$fitted)     
plot(rstudent(mod3)~mod3$fitted)     
plot(rstudent(mod4)~mod4$fitted)     
plot(rstudent(mod5)~mod5$fitted) 

anova(mod0,mod1,mod2,mod3,mod4,mod5)
```

```{r, echo=FALSE}
anovatable = matrix(nrow=6, ncol=6)     
rownames(anovatable) = c("mod0", "mod1", "mod2", "mod3", "mod4", "mod5")     
colnames(anovatable) = c("Res.Df", "RSS", "Df", "Sum of Sq", "F", "Pr(>F)")

an = anova(mod0,mod1,mod2,mod3,mod4,mod5)

anovatable[,1] = an$Res.Df
anovatable[,2] = an$RSS
anovatable[,3] = an$Df
anovatable[,4] = an[4][,1]
anovatable[,5] = an$F
anovatable[,6] = an$Pr

anovatable

```

Looking at the anova table above, it appears that mod2 fits the data well better than the other models. However, not all off the coefficients in this model are significant. This can be due to the small sample sizes of each station as well as outliers and leverage points. I will analyze the outliers of all the models in an attempt to achieve better fits. However, mod2 is currently the model of choice because of its relatively high adjusted R^2, the analysis of deviance table, and the relatively low RSS. I have excluded mod1 from further analysis, because none of its coefficients were significant.

I will next observe the outliers from each of model (excluding mod0 and mod1 for reasons mentioned above), and then remove them in order to create a better fit.

**Plot 1.4a**

```{r, echo=FALSE}

par(mfrow=c(2,2))


plot(rstudent(mod2)~mod2$fitted, main = "Studentized Residuals for Mod2", xlab = "Fitted Values", ylab = "Studentizd Residuals")     
abline(h=0)

plot(rstudent(mod3)~mod3$fitted, main = "Studentized Residuals for Mod3", xlab = "Fitted Values", ylab = "Studentizd Residuals")  
abline(h=0)


plot(rstudent(mod4)~mod4$fitted, main = "Studentized Residuals for Mod4", xlab = "Fitted Values", ylab = "Studentizd Residuals")   
abline(h=0)


plot(rstudent(mod5)~mod5$fitted, main = "Studentized Residuals for Mod5", xlab = "Fitted Values", ylab = "Studentizd Residuals") 
abline(h=0)

```

In each graph above, there are noticeable outliers. I will remove residuals greater than 2 and -2 from each model. Each updated model will be noted (i.e, "mod2update"). Then to further validate the model selection, I will look at other criteria, such as AIC, BIC, and Mallow's Cp to choose the best model.


```{r, echo=FALSE}
checkobs2 = which(abs(rstudent(mod2))>2) 
#62  72  73 107 117 121 136 141 161

checkobs3 = which(abs(rstudent(mod3))>2) 
#12  62  66  73  76 117 136 141 147 161 


checkobs4 = which(abs(rstudent(mod4))>2) 
#12 117 121 133 134 139 141 156 


checkobs5 = which(abs(rstudent(mod5))>2) 
#12  13  14 117 133 141

     
```





```{r, echo=FALSE, include=FALSE}
#calculating hat values
#Model2 

hat.vals2  = hatvalues(mod2)
dfmodel=5
dfErr = nrow(data) - dfmodel
hbar = dfmodel/nrow(data)

par(mfrow=c(1,2))
cooksD2 = cooks.distance(mod2)

which(cooksD2>4/dfErr)
#3  62  67  68  72  73  99 100 136 141 144 147 





plot(cooksD2, main = "Cook's Distance for Mod2", ylab = "Cook's Distance")
abline(h = 4/dfErr)
plot(mod2,3)
plot(mod2,4)     
abline(h = 4/dfErr, col="red")


###Mod3
hat.vals3  = hatvalues(mod3)
dfmodel=4
dfErr = nrow(data) - dfmodel
hbar = dfmodel/nrow(data)

par(mfrow=c(1,2))
cooksD3 = cooks.distance(mod3)

checkobs3 = which(cooksD3>4/dfErr)
#12  62  66  73  76  98 136 141 144 147 161


plot(cooksD3, main = "Cook's Distance for Mod3", ylab = "Cook's Distance")
abline(h = 4/dfErr)
plot(mod3,3)
plot(mod3,4)     
abline(h = 4/dfErr, col="red")



#Mod4
hat.vals4  = hatvalues(mod4)
dfmodel=3
dfErr = nrow(data) - dfmodel
hbar = dfmodel/nrow(data)

par(mfrow=c(1,2))
cooksD4 = cooks.distance(mod4)

which(cooksD4>4/dfErr)
#12  13  39  97 133 141


plot(cooksD4, main = "Cook's Distance for Mod3", ylab = "Cook's Distance")
abline(h = 4/dfErr)
plot(mod4,3)
plot(mod4,4)     
abline(h = 4/dfErr, col="red")



#Mod5
hat.vals5  = hatvalues(mod4)
dfmodel=2
dfErr = nrow(data) - dfmodel
hbar = dfmodel/nrow(data)

par(mfrow=c(1,2))
cooksD5 = cooks.distance(mod5)

which(cooksD5>4/dfErr)
#11  12  13  14  39  75 117 132 133 141 

plot(cooksD5, main = "Cook's Distance for Mod3", ylab = "Cook's Distance")
abline(h = 4/dfErr)
plot(mod5,3)
plot(mod5,4)     
abline(h = 4/dfErr, col="red")




```


```{r, echo=FALSE, include=FALSE}
 
data2=data[-checkobs2,]



mod2update = lm(data2$logmerc ~ data2$logweight + data2$loglength +  as.factor (data2$station)   + data2$logweight:factor(data2$station))

summary(mod2update)



data3=data[-checkobs3,]



mod3update = lm(data3$logmerc ~ data3$logweight + data3$loglength +  as.factor (data3$station))

summary(mod3update)



data4=data[-checkobs4,]



mod4update = lm(data4$logmerc ~ data4$logweight + data4$loglength)

summary(mod4update)

data5=data[-checkobs5,]



mod5update = lm(data5$logmerc ~ data5$logweight)

summary(mod5update)


modNamesupdate = c("mod0", "mod1", "mod2update", "mod3update", "mod4update", "mod5update")
#Figuring out AIC based on the models:
aicout = t(sapply(modNamesupdate, function(name){
extractAIC(get(name))
}))

#Finding BIC
bicout = t(sapply(modNamesupdate, function(name){
extractAIC(get(name), k = log(c(171,171, 162,161,163,165)))}))

bict = t(t(diag(bicout[,2:7])))
rownames(bict) = modNamesupdate

aicbictable =matrix(nrow = 6, ncol =3)
colnames(aicbictable) = c("Df", "AIC", "BIC")
rownames(aicbictable) = modNamesupdate
     
aicbictable[,1] = aicout[,1]
aicbictable[,2] = aicout[,2]
aicbictable[,3] = bicout[,2]


```
The AIC and BIC tables are provided below. Notice that mod2 and mod3 have the lowest levels for both compared to the rest of the models. 

```{r, echo=FALSE}
aicbictable
```
**Discussion:**

By observing a plot of Mallow's Cp below (**Plot 1.4b**), we can see that Mod3,Mod4 and Mod5 have the lowest value. 

**Plot 1.4b**

```{r, echo=FALSE}
    
#plot model selection criteria against model size
twoPlots = function(crit, main) {
  axLab = c("mod0", "mod1","mod2", "mod3", "mod4", "mod5")
ylab = "Criteria"
xlab = "Model"

par(mfrow=c(1,2))  
plot(crit, ylab = ylab, xlab = xlab,
  main = main, xaxt = "n")
axis(1, at = 1:6, axLab)

plot(3:6, crit[ 3:6 ], ylab = ylab, xlab = xlab,
  main = main, xaxt = "n")
axis(1, at = 3:6, axLab[3:6])
}

        
#Mallow's Cp
sig2hatFull = summary(mod2update)$sigma^2

cpout = sapply(modNamesupdate, function(name){
  obj = get(name)
  modDF = length(obj$coefficients)
  2*modDF - c(171,171,162,161,163,165) + summary(obj)$sigma^2 /sig2hatFull
})

cp = t(t(diag(cpout)))

twoPlots(cp, "Mallow's Cp")
````
This leads us to the question, of which model we should choose? Mod2update has the highest R^2 and one of the lowest AIC and BIC values. However, many of its coefficients are NOT significant, which signifies that there is over-fitting. This can explain why it's Mallow's Cp value is not as low as the others. Mod4update and mod5update have the lowest Mallow's Cp values, however, they do not accurately describe the model. Both the models have very low adjusted R^2, and very high residual standard errors (0.4295 and 0.4797 respectively). Mod3 has one of the lowest AIC values and the lowest BIC value. In addition, it also has the second lowest residual standard error (behind mod2update), and nearly all of its coefficients are significant. Also, it has a relatively high adjusted R^2 value of 0.7496. Thus for these reasons, I believe mod3update is the best model. The table below describes these findings.





```{r, echo=FALSE}

b=matrix(nrow=6, ncol=7)


colnames(b) = c("Df", "DfModel", "AIC", "BIC", "Cp", "Adjusted R^2", "Residual Standard Error")

rownames(b) = modNamesupdate

b[,1] = aicbictable[,1]
b[,2] = c(mod0$df, mod1$df, mod2update$df, mod3update$df, mod4update$df, mod5update$df)

b[,3:4] = aicbictable[,2:3]

b[,5] = cp

b[,6] = t(sapply(modNamesupdate, function (x){
  summary(get(x))$adj.r.squared}))
  
  
b[,7] = t(sapply(modNamesupdate, function (x){
  summary(get(x))$sigma}))
  
b


```
In order to verify that the mod3update is a great linear fit, I analyze its QQ plot and its residuals (**Plot 1.5**).

**Plot 1.5**


```{r,echo=FALSE}
par(mfrow=c(1,2))
plot(mod3update,2, main ="QQ Plot of Mod3Update")
plot(mod3update$resid~mod3update$fitted, main="Residuals Vs Fitted Values")
abline(lm(mod3update$resid~mod3update$fitted), col="red")

```
The QQ plot above follows a linear slope, and the residuals appear to have no patterns with the fitted values. In addition, the mean of the residuals is zero. Thus, mod3update appears to fit the data well.



**Conclusion:**
Upon looking at our model and analysis, we found that mercury levels in the fish were best described by regressing on the log of weight, the log of length, and the location of the fish in the river (station). We also noticed through our analysis that a majority of fish that were smaller, tended to have safer amounts of mercury as compared to larger fish (**Plot 1.2a**). Fish in certain locations, such as station 10 and 15 seemed to have higher amounts of mercury than other locations (**Plot 1.2b**). However, despite all this, I believe this model will do a great job in explaining the mercury levels for largemouth bass.


**APPENDIX:**

**Table 1.1**


```{r, echo=FALSE}

a = as.matrix(fishsafeper)
rownames(a) =row_Names 
colnames(a) = "Percentage of Fish < 2 MG of Mercury"
a = as.table(a)

a     

```

**Table 1.2**

```{r,echo=FALSE}
g = matrix (nrow = 6, ncol=2)
colnames(g) = c("Parameters", "Interaction Terms")
rownames(g) = modNames

g[1,] = c("Log of Weight, Log of length, river, station", "logweight and river; logweight and station; loglength and river; loglength and station")

                    
g[2,] = c("Log of Weight, Log of length, station", "loglength and station; logweight and safety; loglength and safety")

g[3,] = c("Log of Weight, Log of length, station", "logweight and station")

g[4,] = c("Log of Weight, Log of length, station, safety", "No Interaction")

g[5,] = c("Log of Weight, Log of length", "No Interaction")

g[6,] = c("Log of Weight", "No Interaction")
          
g = as.table(g)          
          
g          
          

```


