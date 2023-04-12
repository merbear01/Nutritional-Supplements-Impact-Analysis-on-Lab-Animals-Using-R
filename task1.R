library(ggplot2)
#Task 1

#the rnorm() function takes a sample size as input and generates many random
#numbers. It generates a random number using a normal(bell curve) distribution.
#here we are using the rnorm function to create the data for mice.
mice_bfore <- rnorm(200, mean=20, sd=sqrt(2))
mice_after <- rnorm(200, mean=21,sd=sqrt(2.5))
# Combine multiple functions into a single function returning a named vector 
#of outputs. Note: you cannot supply additional parameters for the summary functions
#The rep() is a built-in R function that repeats elements of an object a 
#specified number of times
mice_df <- data.frame(weight =c(mice_bfore,mice_after), time = rep(c("Before",
"After"), each=200),animal=rep("Mice", 400))


#we then have to repeat this but for rats and using the variables given in our
#software requirements.
rats_bfore <- rweibull(200, shape=10,scale=20)
rats_after <- rweibull(200,shape=9,scale=21)
#A Weibull distribution is a continuous probability distribution used to analyze
#life data, model failure times, and access product reliability when modern 
#machines were not available during the olden times.
rats_df <- data.frame(weight = c(rats_bfore, rats_after), time = rep(c("Before",
"After"), each = 200), animal = rep("Rats", 400))

# Combine data
# The rbind function in R, short for row-bind, can be used to combine vectors, 
#matrices and data frames by rows.
r_m_data <- rbind(mice_df, rats_df)

#Next we plot the density using the data r_m_data
qplot(data = r_m_data, x = weight, fill = time, facets = . ~ animal, 
      geom = "density", alpha = I(0.5))

# And finally, we make boxplots for this data
qplot(data = r_m_data, x = time, y = weight, fill = time, 
facets = . ~ animal, geom = "boxplot")

#task 2

# we have to examine	whether	the	data	passes	
# normality	qualitatively	(QQ	plot)	and	quantitatively	(Shapiro-Wilk	test)
# and we have to do this using a combined data set of before and after for the 
#mice and rat data
mice_bfore <- rnorm(200, mean = 20, sd = sqrt(2))
mice_after <- rnorm(200, mean = 21, sd = sqrt(2.5))
mice <- c(mice_bfore, mice_after)

#performing the qqplot to draw the correlation between the given sample and the
# normal distribution.
qqnorm(mice)
qqline(mice)
#and then the Shapiro wilk test for the mice (speak more on this)
shapiro.test(mice)
# we do the same as we did for the mice but for the rats
rats_bfore <- rweibull(200, shape = 10, scale = 20)
rats_after <- rweibull(200, shape = 9, scale = 21)
rats <- c(rats_bfore, rats_after)
qqnorm(rats)
qqline(rats)
shapiro.test(rats)

#task 3
#we have to do a paired t-test and to be able to do it, we would have to first
#split the the data into two groups
mice_bfore <-rnorm(200, mean=20, sd=sqrt(2))
mice_after <-rnorm(200, mean=21, sd=sqrt(2.5))

# after we split the data into the groups before and after, we would then need
#to find the difference between the two groups
mice_difference <- mice_after - mice_bfore

#and finally, we do the paired t-test
t.test(mice_after,mice_bfore,paired=TRUE)

#and we need to perform a non-parametric test on the rats data so we decide to use the 
#wilcoxon signed rank test
#therefore, we have to split the data first
rats_bfore <- rweibull(200, shape=10, scale=20)
rats_after <- rweibull(200, shape=9, scale=21)

#next we need to find the difference 
rats_difference <- rats_after - rats_bfore

#And finally we do the wilcoxon signed rank test
wilcox.test(rats_bfore, rats_after, paired=TRUE)

#task4
#in our last task we need to use the 'fitdist' package on the rats dataset, 
#examining the best-fit distribution. we need to get the weibull, lognormal and 
#a gamma distribution report on the dataset.
library(fitdistrplus)
library(ggplot2)
#next we combine the the before and after packages of the rat
rats_data <- c(rweibull(200, shape = 10, scale = 20), rweibull(200, shape = 9,
                                                               scale = 21))

# next we need to get the distribution fitting right
weibull_fit <- fitdist(rats_data, "weibull")
summary(weibull_fit)
lognorm_fit <- fitdist(rats_data, "lnorm")
summary(lognorm_fit)
gamma_fit <- fitdist(rats_data, "gamma")
summary(gamma_fit)

#then we create the comparison plot for Density
denscomp(list(weibull_fit, lognorm_fit, gamma_fit),
         legendtext = c("Weibull", "Lognormal", "Gamma"))



# we next have to do a plot for the CDF
cdfcomp(list(weibull_fit, lognorm_fit, gamma_fit),
        legendtext = c("Weibull", "Lognormal", "Gamma"))


# Then for the qq plot
qqcomp(list(weibull_fit, lognorm_fit, gamma_fit),
       legendtext = c("Weibull", "Lognormal", "Gamma"))

# and finally the pp plot
ppcomp(list(weibull_fit, lognorm_fit, gamma_fit),
       legendtext = c("Weibull", "Lognormal", "Gamma"))

