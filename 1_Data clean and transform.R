library(dplyr)#φορτώνουμε τις βιβλιοθήκες
library(stringr)
library(tidyr)
library(readr)
library(tidytext)
library(textdata)

#φορτώνουμε τα raw data
load("IMDB_Final_5000_Movies.RData")

#βγάζουμε τα $ και κρατάμε μόνο τους αριθμούς
clean_money <- function(x) {
  x <- as.character(x)
  x <- str_remove_all(x, "[^0-9]")
  as.numeric(x)
}
#μετατρέπουμε τα K και M σε κανονικά νούμερα
clean_counts <- function(x) {
  x <- as.character(x)
  x <- str_remove_all(x, ",")
  multiplier <- rep(1, length(x))
  multiplier[str_detect(x, "K")] <- 1000
  multiplier[str_detect(x, "M")] <- 1000000
  val <- as.numeric(str_extract(x, "[0-9.]+"))
  return(val * multiplier)
}
#κάνουμε parse τη διάρκεια
clean_duration <- function(x) {
  hours <- as.numeric(str_extract(x, "[0-9]+(?=h)"))
  minutes <- as.numeric(str_extract(x, "[0-9]+(?=m)"))
  hours[is.na(hours)] <- 0
  minutes[is.na(minutes)] <- 0
  total_minutes <- (hours * 60) + minutes
  total_minutes[total_minutes == 0] <- NA
  return(total_minutes)
}
#βαζουμε τις συναρτήσεις πισω στο dataframe μας
clean_df <- final_df %>%
  mutate(
    year = as.numeric(str_extract(year, "[0-9]{4}")),
    imdb_rating = as.numeric(imdb_rating),
    metascore = as.numeric(metascore),
    duration_mins = clean_duration(duration),
    budget_clean = clean_money(budget),
    gross_world_clean = clean_money(gross_worldwide),
    gross_us_clean = clean_money(gross_us_canada),
    opening_weekend_clean = clean_money(opening_weekend),
    user_reviews_clean = clean_counts(user_reviews_count),
    critic_reviews_clean = clean_counts(critic_reviews_count)
  )
sentiment_scores <- clean_df %>%
  select(id, review_samples) %>%
  unnest_tokens(word, review_samples) %>%
  inner_join(get_sentiments("bing")) %>%
  count(id, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment_score = positive - negative) %>%
  select(id, sentiment_score)
clean_df <- clean_df %>%
  left_join(sentiment_scores, by = "id") %>%
  mutate(sentiment_score = ifelse(is.na(sentiment_score), 0, sentiment_score)) #οπου δεν βρήκε λέξεις εβάλε 0

#φτιάχνουμε μια λίστα με όλα τα genre που υπάρχουν
all_genres_list <- clean_df %>%
  separate_rows(genres, sep = ", ") %>%
  pull(genres) %>%
  unique() %>%
  na.omit()
#πετάμε τα κενά ή NA
all_genres_list <- all_genres_list[all_genres_list != "NA" & all_genres_list != ""]
#φτιάχνουμε μια στήλη για κάθε είδος (0 ή 1) για την παλινδόμηση
for(g in all_genres_list) {
  col_name <- paste0("Genre_", str_replace_all(g, "[^a-zA-Z]", "")) 
  clean_df[[col_name]] <- as.integer(str_detect(clean_df$genres, g))
}
#βαζουμε μόνο τις στήλες που θα χρησιμοποιηθούν στο μοντέλο
model_data <- clean_df %>%
  select(
    title, year, duration_mins, imdb_rating, metascore,
    pg_rating, user_reviews_clean, critic_reviews_clean,
    budget_clean, gross_world_clean, gross_us_clean, opening_weekend_clean,
    sentiment_score,
    starts_with("Genre_") 
  )
model_data$pg_rating <- as.factor(model_data$pg_rating)
#φιλτράρουμε τις ταινίες που δεν έχουν budget ή gross
regression_ready_df <- model_data %>%
  filter(!is.na(gross_world_clean)) %>% 
  filter(!is.na(budget_clean))

print(paste("cleaned Rows ready for model:", nrow(regression_ready_df)))

save(regression_ready_df, file = "Regression_Ready_Data.RData")
write.csv(regression_ready_df, "Regression_Ready_Data.csv", row.names = FALSE)