library(dplyr)#φορτώνουμε τις βιβλιοθήκες
library(caret)
library(ggplot2)

#φορτώνουμε τα καθαρά δεδομένα
load("Regression_Ready_Data.RData")

#αυτές είναι οι μεταβλητές που θεωρώ επηρεάζουν το σκορ άρα τις βαζω στο model
model_data <- regression_ready_df %>%
  select(
    imdb_rating,            
    duration_mins,          
    year,                   
    metascore,             
    user_reviews_clean,     
    critic_reviews_clean,   
    budget_clean,          
    gross_us_clean,
    opening_weekend_clean,
    sentiment_score,
    pg_rating,        
    Genre_Drama,           
    Genre_Action,         
    Genre_Comedy,           
    Genre_Horror,          
    Genre_Animation,        
    Genre_SciFi             
  ) %>%
  na.omit() #βγαζουμε τα κενά για να μην κρασάρει

print(paste("Rows used for model:", nrow(model_data)))

#σετάρουμε το 5 fold Cross Validation, τρεχοντας το μοντέλο 5 φορές για να είμαστε σίγουροι
train_control <- trainControl(method = "cv", number = 5)

#χωρίζουμε τα δεδομένα 70% train και 30% test, τα set seeds τελείως τυχαιο νούμερο
set.seed(62) 
train_index <- createDataPartition(model_data$imdb_rating, p = 0.7, list = FALSE)
train_data <- model_data[train_index, ]
test_data  <- model_data[-train_index, ]

#Linear Regression
set.seed(146)
model_lm <- train(
  imdb_rating ~ ., 
  data = train_data, 
  method = "lm",           
  trControl = train_control
)

#Random Forest
set.seed(123)
model_rf <- train(
  imdb_rating ~ ., 
  data = train_data, 
  method = "ranger",
  trControl = train_control,
  importance = 'impurity'  #για να δούμε ποιες μεταβλητές είναι σημαντικές
)

#σύγκριση μοντέλων
results <- resamples(list(LinearReg = model_lm, RandomForest = model_rf))
summary(results)
#κάνουμε predict στο test set στις ταινίες που το μοντέλο δεν έχει δει ποτέ
pred_lm <- predict(model_lm, newdata = test_data)
pred_rf <- predict(model_rf, newdata = test_data)
#υπολογίζουμε το Root Mean Square Error
rmse_lm <- sqrt(mean((test_data$imdb_rating - pred_lm)^2))
rmse_rf <- sqrt(mean((test_data$imdb_rating - pred_rf)^2))

print(paste("Final Test Root Mean Square Error - Linear Regression:", round(rmse_lm, 3)))
print(paste("Final Test Root Mean Square Error - Random Forest:    ", round(rmse_rf, 3)))

#βλέπουμε ποιο μοντέλο κέρδισε για να φτιάξουμε το τελικό γράφημα
best_preds <- if (rmse_rf < rmse_lm) pred_rf else pred_lm
best_name  <- if (rmse_rf < rmse_lm) "Random Forest" else "Linear Regression"

results_plot <- data.frame(Actual = test_data$imdb_rating, Predicted = best_preds)
ggplot(results_plot, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_abline(color = "red") +
  ggtitle(paste("Best Model Predictions:", best_name)) +
  theme_minimal()

rf_importance <- varImp(model_rf, scale = FALSE)
print(plot(rf_importance, top = 15, main = "Ποιοι παράγοντες επηρεάζουν το IMDB Score;"))
summary(model_lm)$coefficients
