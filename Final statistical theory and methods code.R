createsample(38)
summary(mysample)
str(mysample)
# No serial killers born before 1900 and therefore not killed before then. 

#Does the average age at first murder differ between killers with different motives?
#Data cleaning: 
#removing any rows with missing observations for either age at first kill 
#(missing numerical values are recorded as 99999) or motive (missing motives are recorded as NA). Also remove any rows containing the data of killers who first killed before the year 1900.removing any rows with missing observations for either age at first kill (missing numerical values are recorded as 99999) or motive (missing motives are recorded as NA). 
#Also remove any rows containing the data of killers who first killed before the year 1900.



mysample1 <- subset.data.frame(mysample, mysample$YearBorn>1900) #change year born
mysample2 <- subset.data.frame(mysample1, mysample1$AgeFirstKill!=99999) #remove NA AgeFirstKillValues
mysample3 <- subset.data.frame(mysample2, mysample2$Motive!=is.na(mysample2$Motive)) 
str(mysample) #254 observations
str(mysample3) #234 observations

CareerDuration <- (mysample3$AgeLastKill-mysample3$AgeFirstKill)
mysample4 <- cbind(mysample3, CareerDuration) #adding and defining career duration. 
mysample4

#checking the number of values I removed 
bornearly <- mysample[mysample$YearBorn <=1900,] #removed 6 or 2.36% of observations
str(bornearly)
NoAgeFirstKill <- mysample1[mysample1$AgeFirstKill ==99999,]
str(NoAgeFirstKill) # removed 8 observations or 3.15 % of observations
NoMotive <- mysample2[mysample2$Motive == is.na(mysample2$Motive),]
str(NoMotive) #Removed 6 values or 2.36% 


#Data exploration
#Numerically and graphically summarise the distribution of three variables: 
#Age at first kill, age at last kill, and career duration


# boxplot summary

boxplot(mysample4$CareerDuration, mysample4$AgeLastKill, mysample4$AgeFirstKill,
        cex.axis = 2,
        cex.names = 2,
        col = "steelblue",
        border ="black",
        notch = TRUE,
        pch = 16,
        names = c("Career Duration", "Age of Last Kill", "Age of First Kill"),
        horizontal = TRUE)


#FIRST KILL ~ LAST KILL - If you kill early, does this mean your last kill will happen later?



library(ggplot2)

df <-mysample4
df$pca1 <- predict(prcomp(~AgeFirstKill+AgeLastKill, df))[,1] #coloring by correlation (principal component analysis)

ggplot(df, 
       aes(AgeFirstKill, AgeLastKill, color = pca1)) + 
  geom_point(shape = 16, size = 6, show.legend = FALSE, alpha = 1) + 
  theme(axis.title=element_text(size=22,face="bold"), 
        plot.title = element_text(size=30, face = "bold"),
        axis.text.x = element_text(face="bold", size=14),
        axis.text.y = element_text(face="bold", size=14)) +
  scale_color_gradient(low = "#0091ff", high = "#f0650e") +
  labs(title = "Correlation Between Age of First and Last Kill", 
  x = "Age of First Kill", y = "Age of Last Kill")+
  geom_smooth(method='lm')


#RELATIONSHIP BETWEEN FIRST KILL AND CAREER DURATION: does age of 1st kill determine career duration?  

df$pca2 <- predict(prcomp(~AgeFirstKill+CareerDuration, df))[,1] #coloring by correlation (principal component analysis)

ggplot(df, 
       aes(AgeFirstKill, CareerDuration, color = pca2)) + 
  geom_point(shape = 16, size = 6, show.legend = FALSE, alpha = 2) + 
  theme(axis.title=element_text(size=22,face="bold"), 
        plot.title = element_text(size=30, face = "bold"),
        axis.text.x = element_text(face="bold", size=14),
        axis.text.y = element_text(face="bold", size=14)) +
  scale_color_gradient(low = "#0dfff3", high = "#20912b") +
  labs(title = "Correlation Between Age of First Kill and Career Duration", 
       x = "Age of First Kill", y = "Career Duration") +
  geom_smooth(method='lm') +
  ylim(0,40)+
  scale_y_continuous(minor_breaks = seq(0 , 40, 2), breaks = seq(0, 40, 10))+ 
  scale_x_continuous(minor_breaks = seq(0 , 70, 5), breaks = seq(0, 70, 10))


#RELATIONSHIP BETWEEN LAST Kill  AND CAREER DURATION: does age of last kill explain career duration? 

df <- mysample4
df$pca3 <- predict(prcomp(~AgeLastKill+CareerDuration, df))[,1] #coloring by correlation (principal component analysis)

ggplot(df, 
       aes(AgeLastKill, CareerDuration, color = pca3)) + 
  geom_point(shape = 16, size = 6, show.legend = FALSE, alpha = 2) + 
  theme(axis.title=element_text(size=22,face="bold"), 
        plot.title = element_text(size=30, face = "bold"),
        axis.text.x = element_text(face="bold", size=14),
        axis.text.y = element_text(face="bold", size=14)) +
  scale_color_gradient(low = "#0dff2d", high = "#d62d2d") +
  labs(title = "Correlation Between Age of First Kill and Career Duration", 
       x = "Age of Last Kill", y = "Career Duration") +
  geom_smooth(method='lm') +
  scale_y_continuous(minor_breaks = seq(0 , 40, 2), breaks = seq(0, 40, 10))+ 
  scale_x_continuous(minor_breaks = seq(0 , 70, 5), breaks = seq(0, 70, 10))
  

#Probability Density Estimation: 


#Histograms for Age of First Kill: 

AFKH <- function(bw) {
  ggplot(df, aes(AgeFirstKill,fill = cut(AgeFirstKill, 100))) +
    geom_histogram(aes(y=..count../sum(..count..)/bw), show.legend = FALSE, 
                   binwidth = bw) +
    scale_fill_discrete(h = c(200, 10), c = 100, l = 60) + 
    theme(axis.title=element_text(size=22,face="bold"), 
          plot.title = element_text(size=30, face = "bold"),
          axis.text.x = element_text(face="bold", size=14),
          axis.text.y = element_text(face="bold", size=14)) +
    labs(title = "Age of First kill",x = "Age of First Kill", y = "Density") +
    geom_density(aes(AgeFirstKill, y = ..density..), alpha = 0.5, 
                 fill = "white",lwd =1, stat = "density")
}

AFKH(bw=5)


#test to see if normally distributed using a Q-Q plot
ggplot(df, mapping = aes(sample = AgeFirstKill)) +stat_qq() +stat_qq_line()+
theme(axis.title=element_text(size=22,face="bold"), 
      plot.title = element_text(size=30, face = "bold"),
      axis.text.x = element_text(face="bold", size=14),
      axis.text.y = element_text(face="bold", size=14)) +
  labs(title = "QQ-plot: Age of First Kill",x = "Theoretical Quantiles", y = "Sample quantiles")
#test to see if normally distributed using a Q-Q plot 
#heavy tailed distributions? 



 #Histograms for Age of Last Kill: 

ALKH <- function(bw) {
  ggplot(df, aes(AgeLastKill,fill = cut(AgeLastKill, 100))) +
    geom_histogram(aes(y=..count../sum(..count..)/bw), show.legend = FALSE, 
                   binwidth = bw) + 
  scale_fill_discrete(h = c(70, 210), c = 50, l = 50) + 
  theme(axis.title=element_text(size=22,face="bold"), 
        plot.title = element_text(size=30, face = "bold"),
        axis.text.x = element_text(face="bold", size=14),
        axis.text.y = element_text(face="bold", size=14)) +
  labs(title = "Age of Last kill",x = "Age of Last Kill", y = "Density")+
    geom_density(aes(AgeLastKill, y = ..density..), alpha = 0.5, 
                 fill = "white",lwd =1, stat = "density")
}

ALKH(bw=5)


#test to see if normally distributed using a Q-Q plot 
ggplot(df, mapping = aes(sample = AgeLastKill)) +stat_qq() +stat_qq_line()+
theme(axis.title=element_text(size=22,face="bold"), 
      plot.title = element_text(size=30, face = "bold"),
      axis.text.x = element_text(face="bold", size=14),
      axis.text.y = element_text(face="bold", size=14)) +
  labs(title = "QQ-plot: Age of Last kill",x = "Theoretical Quantiles", y = "Sample quantiles")




#Histogram of Career Duration:


CDH <- function(bw) {
  ggplot(df, aes(CareerDuration,fill = cut(CareerDuration, 100))) + 
  geom_histogram(aes(y=..count../sum(..count..)/bw), show.legend = FALSE, 
                 binwidth = bw) +
  scale_fill_discrete(h = c(180, 360), c = 100, l = 60) + 
  theme(axis.title=element_text(size=22,face="bold"), 
        plot.title = element_text(size=30, face = "bold"),
        axis.text.x = element_text(face="bold", size=14),
        axis.text.y = element_text(face="bold", size=14)) +
  labs(title = "Histogram of Career Duration",x = "Career Duration", y = "Density")+
  geom_density(alpha = 0.5, fill = "white",lwd =1)
}

CDH(bw=3)


# doing a q-q plot for an exponential distribution 

library(MASS)
params <- as.list(fitdistr(CareerDuration, "exponential")$estimate)
#converts objects to lists, 
#then Maximum-likelihood fitting of univariate distributions
ggplot(df, mapping = aes(sample = CareerDuration)) +
  stat_qq(distribution = qexp, dparams = params)+ # changes the distribution parameters, mainly alters x-axis. 
  stat_qq_line(distribution = qexp, dparams = params) + # adds the line of best fit (i.e. if it fits an expontential distribution)
  theme(axis.title=element_text(size=22,face="bold"), 
        plot.title = element_text(size=30, face = "bold"),
        axis.text.x = element_text(face="bold", size=14),
        axis.text.y = element_text(face="bold", size=14)) +
  labs(title = "QQ-plot: Career Duration",x = "Theoretical Quantiles", y = "Sample quantiles")




#getting the pdf of Career Duration

library(ggplot2)
exppdf <-ggplot(df, aes(CareerDuration)) +
  geom_density(color="darkblue", fill="lightblue")
exppdf

#testing the Age of first kill

agefirstkilldf <-ggplot(df, aes(AgeFirstKill)) +
  geom_density(color="darkblue", fill="lightblue")
agefirstkilldf

#testing the age of last kill

agelastkilldf <-ggplot(df, aes(AgeLastKill)) +
  geom_density(color="darkblue", fill="lightblue")
agelastkilldf




#using a Shapiro-Wilk test to test for normality: 
install.packages("dplyr")
library(dplyr)
shapiro.test(mysample4$AgeFirstKill)

#using a Shapiro-Wilk test to test for normality: 
shapiro.test(mysample4$AgeLastKill)

#this is definitely an exponential distribution. test using Kolmogorov-Smirnov test?  
install.packages("exptest")
library(exptest)
shapiro.exp.test(mysample4$CareerDuration)


#Likelihood estimation - we have values in a sample and we want to find out what the
#value of the parameter which makes the observed data most likely to occur. 

 

# PARAMETER ESTIMATIon 

# Career Duration - fairly sure it is exponential. As a result, use maximum likelihood estimator. 

# just comment on if it is bias or not. 

#estimaing
install.packages("EnvStats")
library(EnvStats)
eexp(mysample4$CareerDuration, method = "mle", ci= TRUE) # the estimator tends to overestimate 

#same for age of first kill - appears to be normal
mean(mysample4$AgeFirstKill)
var(mysample4$AgeFirstKill)

#same for age of last kill - appears to be normal
mean(mysample4$AgeLastKill)
var(mysample4$AgeLastKill)

install.packages("SimDesign")
library(SimDesign)


#5 - Hypothesis testing: 

#means
escape<- mysample4$AgeFirstKill[mysample4$Motive== "Escape or avoid arrest"] # mean of age of first kill when motive is escape or avoid arrest
mean(escape)
anger<- mysample4$AgeFirstKill[mysample4$Motive== "Anger (including mission-oriented killers)"] # mean of age of first kill when motive is Anger (including mission-oriented killers)
mean(anger)
conv<- mysample4$AgeFirstKill[mysample4$Motive== "Convenience (didn't want children/spouse)"] # mean of age of first kill when motive is Convenience (didn't want children/spouse)
mean(conv)


par(mfrow = c(2,2))

#normality of age first kill when motive is escape: 
qqnorm(escape, pch =1, frame = FALSE, main = "Quantile-Quantile plot of the age 
of first kill motivated by escaping/avoiding arrest")
qqline(escape, col = "steelblue", lwd =2)
str(escape)

#normality of age first kill when motive is convenience: 
qqnorm(conv, pch =1, frame = FALSE, main = "Quantile-Quantile plot to test if age of 
first kill motivated by convienience")
qqline(conv, col = "steelblue", lwd =2)
str(conv)
var(conv)

#normality of age first kill when motive is anger: # this indicates that it is skewed right.  
qqnorm(anger, pch =1, frame = FALSE, main = "Quantile-Quantile plot to test 
if age of first kill motivated by anger")
qqline(anger, col = "steelblue", lwd =2)
str(anger)


m<-mean(anger)
std<-sqrt(var(anger))
hist(anger, density=90, breaks=10, prob=TRUE, xlim=c(10,70), xlab="age of first kill", main="histogram of age of first kill motivated by anger")
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")



MOTIVE: ESCAPE

#For the age of first kill when motive is escape, the QQ plot indicates that this is normally distributed.
#However, as there are only 19 values in this sample, it would be unwise to assume population mean = sample mean.
#Consequently, A one sample t-test was used to determine if there was a significant difference between the sample mean 
#of the age of first killer where the motive was escape and the hypothesized population mean of 27 years (for all motives)

t.test(escape,mu=27)

"similarly, for age of first kill where it is motivated by convienience, although the QQ plot suggests a 
normally distributed random variable, n = 10. Due to this small sample size, the same one sample t-test was performed"
t.test(conv, mu = 27)

#if sample variance is an unbiased estimator for underlying parameter, population variance, then we assume that 
#the expected value of the estimator matches the expected value of the parameter, so does this mean they're equal,
# or is the expectation of theta different to theta?
# does the CLT apply to distribution of sample means, or really large samples. 

#Conversely, although the quantile-quantile plot revealed that the age of first kill motivated by 
#anger is likely a right-skewed, due to the larger sample size (n = 205), it is a justifiable assumption
#that the sample variance was equal to population variance. Furthermore, the larger sample size allows the assumption
#that x is normally distributed or has any particular distribution, based on application of the central limits theorem:
#This however, means that this 95% confidence distribution is an approximation

install.packages("BSDA")
library(BSDA)
z.test(anger,mu =27,sigma.x = sd(anger))
mean(anger)
str(anger)

#comparison of populations: 

# an unpaired t-test was run to compare escape and convenience to see if mean age differs between motives
# this assume independence both between and within the each sample, in this case, normality was assumed (based)
# on the QQ plots generated. However, it was not assumed that sample variances were equal. 

t.test(conv, escape, alternative = "two.sided", var.equal = FALSE)
mean(conv) - mean(escape)

# The results of this T-test indicate there is no significant difference between the means based on the 
# overlap in the confidence intervals. 


#an unpaired t-test was run to compare escape and anger to see if mean age differs between motives

t.test(escape, anger, alternative = "two.sided", var.equal = FALSE)
mean(escape) - mean(anger)

#an unpaired t-test was run to compare escape and anger to see if mean age differs between motives

t.test(conv, anger, alternative = "two.sided", var.equal = FALSE)
mean(conv) - mean(anger)


#forest plot


plot(x = 0,                                 # One point at (0,0).
     xlim = c(-20, 20), ylim=c(0, 5),        # Axis limits.
     type = "n", xaxt = "n", yaxt="n",       # No points, no axes drawn.
     xlab = NULL, ylab= NULL, ann = FALSE,   # No axis labels or numbers.
     bty="n")                                # No box.
# Add a horizontal (side = 1) axis:

axis(side = 1, cex.axis = 1) 


# Add an axis label 4 lines below the axis:

mtext("Estimated difference in the means of motives (95% confidence interval)", 
      side = 1, line = 4)
# Add some grid lines, preferably lined up
# with the numbers on the horizontal axis:

for(i in c(-20, -15, -10, -5, 0, 5, 10, 15, 20)){
  
  lines(c(i, i), c(0, 5), lty = 2, col = "gray53")
  
}
# Add labels for each analysis on the left (side = 2) 
# at vertical heights of 1, 2, 3 and 4:

verticalpos = 1:3

motives <- c(
  " Anger and 
Convenience", 
  
  "  Escape and 
       Anger",
  
  " Escape and 
Convenience")

mtext(text = motives,  at = verticalpos, 
      side = 2, line = 4, outer = FALSE, las = 1, adj = 0)

# Try changing the "line" option to move these closer to 
# or away from the plotted intervals.
# Plot the four point estimates (centres 
# of the CIs for each analysis): 

# Plot the four interval estimates:

estimate  =  c(7.50,4.79,2.71)             
upper     =  c(15.29,8.7,10.99)
lower     =  c(-0.28,0.89,-5.57)
pval      =  c(0.05722,0.01843,0.4945)

points(estimate, verticalpos, pch = 16)

for(i in 1:3){
  lines(c(lower[i], upper[i]), c(verticalpos[i], verticalpos[i]))
  lines(c(lower[i], lower[i]), c(verticalpos[i] + 0.2, verticalpos[i] - 0.2))
  lines(c(upper[i], upper[i]), c(verticalpos[i] + 0.2, verticalpos[i] - 0.2))
}
# Now we add numerical results on the right (side = 4), but we 
# need to put them into a nice form first. Note that
# paste() merges words and numbers, and formatC()
# allows us to control the number of decimal places.

#additonal exploration

angercareer <- mysample4$CareerDuration[mysample4$Motive== "Anger (including mission-oriented killers)"]
summary(angercareer)
escapecareer <- mysample4$CareerDuration[mysample4$Motive== "Escape or avoid arrest"]
summary(escapecareer) 


