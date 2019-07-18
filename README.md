# HR-Analytics - WNS Analytics Hackathon

Problem Statement : Can you predict which employee has a higher chance of getting promoted and help the help the organization to expedite the appraisal and promotion process?
Link : https://datahack.analyticsvidhya.com/contest/wns-analytics-hackathon-2018-1/

About the hackathon : 
HR analytics is revolutionising the way human resources departments operate, leading to higher efficiency and better results overall. Human resources has been using analytics for years. However, the collection, processing and analysis of data has been largely manual, and given the nature of human resources dynamics and HR KPIs, the approach has been constraining HR.The task is to predict whether a potential promotee at checkpoint in the test set will be promoted or not after the evaluation process.The task is to identify the right people for promotion (only for manager position and below) and prepare them in time.

Solution :
The training dataset and test dataset has been clubbed into a common large dataset called data.
The columns "KPIs_met..80." and "awards_won." have been renamed and factored as "Yes"/"No" instead of numeric vector 1/0. 

From the biplot derived from principal component test we understand that age and length_of_service are linear in behaviour followed by (average_training_score & awards_won) and  (KPIGReaterThan80perc & previous_year_rating)

We thus perform feature engineering on the dataset viz., deriving avg_mean_score by department and region, bucketing of age column, derving start_age / joining_age and converting is_promoted variable to factor variable.

Missing or null values are imputed in variables previous_year_rating and education by the mode value. We further scale the score derived from avg_mean_score by department and region.

It is observed that the percentage of employees promoted is less , thereby having an imbalanced dataset. To rectify this, undersampling technique is used and preferred over other sampling methods.

Logistic regression is first performed on the training dataset, from which we learn that dummy variables have to be created for the variables department,region and education to better understand its significance. The dummy variables are created for both training and testing dataset.

On further performing logistic regression and trimming down to significant variables, we attain an accuracy of 79.80
