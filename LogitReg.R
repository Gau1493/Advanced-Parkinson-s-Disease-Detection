train=read.csv('train.csv',strip.white = T)
str(train)
train$class=factor(train$class)
View(train)
model=glm(class~.-subject_id-UPDRS,data=train,family = binomial)
summary(model)

m1=step(model,direction = "both",trace=T)

m2=glm(class ~ jitter_.local.absolute. + shimmer_.local.dB. + shimmer_.apq11. + 
         shimmer_.dda. + AC + max_pitch + no_of_periods + mean_period + 
         frac_locally_unvoiced_frames + degree_of_voice_breaks,data=train,family = binomial)
summary(m2)

test=read.csv("test.csv",strip.white = T)
test$class=factor(test$class)
str(test)

# Load the pROC library for plotting ROC curve
library(pROC)
predicted_probs <- predict(m2, test, type = "response")

roc_curve <- roc(test$class,predicted_probs,print.auc=TRUE,plot=T,legacy.axes=T)


# Create an ROC curve
roc_curve <- roc(test$class, predicted_probs,print.auc=TRUE,plot=T,legacy.axes=T)


# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for Logistic Model")

# Add diagonal line for reference
abline(a=0, b=1, col="gray", lty=2)

# Calculate and print the Area Under the Curve (AUC)
auc_value <- auc(roc_curve)
auc_value
