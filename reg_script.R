library(tidyverse)
library(apaTables)

# Load popuation data
population_data <- read_csv("population_data.csv")

glimpse(population_data)


# get sample
set.seed(1)
sample1_data <- sample_n(population_data,size=200)

glimpse(sample1_data)


# sample regression
sample1_lm_results <- lm(performance ~ IQ + 1, data=sample1_data)
summary(sample1_lm_results)


# population regression
sample1_lm_results_reg_table <- apa.reg.table(sample1_lm_results)
print(sample1_lm_results_reg_table)
summary(sample1_lm_results)


# predicted value for a single person
x_axis_range <- data.frame(IQ = c(120))

CI_data <- predict(sample1_lm_results,newdata=x_axis_range,interval = "confidence", level=.95)
CI_data <- as.data.frame(cbind(x_axis_range,CI_data))
CI_data

# predicted value for entire x-axis range
min_predictor <- min(sample1_data$IQ)
max_predictor <- max(sample1_data$IQ)

min_predictor
max_predictor


x_axis_range <- data.frame(IQ=seq(min_predictor,max_predictor,by=.5))
x_axis_range

CI_data <- predict(sample1_lm_results,newdata=x_axis_range,interval = "confidence", level=.95)
CI_data <- as.data.frame(cbind(x_axis_range,CI_data))
CI_data

PI_data <- predict(sample1_lm_results,newdata=x_axis_range,interval = "prediction", level=.95)
PI_data <- as.data.frame(cbind(x_axis_range,PI_data))
PI_data
head(CI_data)
head(PI_data)

reg_plot <- ggplot(sample1_data,aes(x=IQ,y=performance))
reg_plot <- reg_plot + geom_point()
reg_plot <- reg_plot + theme_classic()

reg_plot <- reg_plot + geom_smooth(data=CI_data,aes(x=IQ,y=fit,ymin=lwr,ymax=upr),stat="identity")

# Or you can use this for the CI
# reg_plot <- reg_plot + geom_smooth(method="lm",se=TRUE)

reg_plot <- reg_plot + geom_smooth(data=PI_data,aes(x=IQ,y=fit,ymin=lwr,ymax=upr),stat="identity")
print(reg_plot)


                           

