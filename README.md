# Multiple.Regression

## Objective
Question 1: Fit the best multiple regression model to the data in test.pollute.txt using pollution as the response variable

Question 2: Fit best multi regression model to data in Daphnia.txt using growth rate as response variable

Question 3: Fit the best multiple regression model to the data in germination.txt with count as the response variable.

## Summary
Question 1:After created my first set of models, I noticed that industry and population were highly correlated. This means I had a problem with multicollinearity. I fixed this by taking out the metric that was least significant to the response variable. My partial model was built from only the predictors that were significant and I transformed the response variable to deal with some of the outliers. Then I remade the models and used stepAIC function to help me find the "best" model. I also did a residual analysis to check my findings. 

Question 2: There were many nominal variables in the data so I had to create several dummy variables to fix this problem. I created a full and partial model, and used to anova table to compare. My partial model was built from only the predictors that were significant. Then I used stepAIC function to confirm that the model I chose what the "best" model. I also did a residual analysis to check my findings

Question 3: Created some dummy variables for nominal varibles in data. Then I created a full and partial model with response varible count. My partial model was built from only the predictors that were significant. I compared my partial model with my full model using anova table. I used the stepAIC funciton to find the "best" model. I also did a residual analysis to check my findings

## Conclusion
Question 1
After dealing with multicollinearity, my final model is: new.part.lm2 (Predictors = temp, industry, wind, rain and response is log(pollute))- Best R squared after dealing with multicollinearity and is built the same as model with best AIC score, except response variable has been transformed because of some outliers in residual plots. It's F test pvalue also says it's significant. 

Question 2
After creating dummy variables. The best model is p.lm.2 model (predictors = clone2 , clone3 and response = growth rate). It has the lowest AIC score and best r squared value. This tells me that best indicator of growth rate is if daphnia is clone2 and/or clone3

Question 3
After creating dummy variables. The best model is p.lm.3 model (predictors = sample , extract and response = count).It has the lowest AIC score and best r squared value. Also pvalue for F test was significantly low. 
