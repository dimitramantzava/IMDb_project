library(rvest)#φορτώνουμε τις βιβλιοθήκες που θα χρειαστούμε για το scraping και τη διαχείριση δεδομένων
library(dplyr)
library(stringr)
library(jsonlite)
library(readr)
#ορίζουμε τα αρχικα ids
queue_ids <- c("tt1014759", "tt1517268", "tt8503618", "tt0327597", "tt1392170") 
visited_ids <- c()

target_limit <- 5000 #όριο ταινιών που θα κανει scrape
options(timeout = 180)
#χρησιμοποίησα User-Agent για να δείξοψ και καλά πως είμαι απλά browser και να μην φαω ban
user_agent_str <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"

get_page_content <- function(link) {
  con <- NULL
  tryCatch({
    con <- url(link, "rb", headers = c("User-Agent" = user_agent_str))
    page <- read_html(con)
    close(con)
    return(page)
  }, error = function(e) {
    if(!is.null(con)) try(close(con), silent=TRUE)
    return(NULL)
  })
}

#αρχίζουμε το μάζεμα δεδομένων
while(length(all_movies_data) < target_limit && length(queue_ids) > 0) {
  current_id <- queue_ids[1]
  queue_ids <- queue_ids[-1]
  
  if (current_id %in% visited_ids) next #διαβεβαίωση πως δεν υπάρχουν doubles από την ίδια ταινία
  #απλά για βα βλέπουμε το progress
  print(paste("Processing:", current_id, "| Queue:", length(queue_ids), "|  Collected So Far:", length(all_movies_data)))
  record <- NULL
  new_ids_found <- c()
  
  tryCatch({
    
    main_link <- paste0("https://www.imdb.com/title/", current_id, "/")
    page_main <- get_page_content(main_link)
    
    if(!is.null(page_main)) {
      json_data <- NULL 
      json_script <- page_main %>% html_node('script[type="application/ld+json"]') %>% html_text()
      if (!is.na(json_script)) json_data <- fromJSON(json_script)
      
      title <- page_main %>% html_node("h1") %>% html_text() %>% str_trim()
      #φτιάχνουμε τα Genres επειδή το JSON μερικε΄ς φορές μου τα έδειχνε περίεργα
      genres <- "NA"
      if (!is.null(json_data$genre)) {
        if(is.list(json_data$genre)) genres <- paste(unlist(json_data$genre), collapse = ", ")
        else genres <- paste(json_data$genre, collapse = ", ")
      } else {
        g_nodes <- page_main %>% html_nodes("a[href*='/genres/']") %>% html_text() #σε περίπτωση που ήταν άδειο, απλά για εναλλακτική
        if(length(g_nodes)>0) genres <- paste(unique(g_nodes), collapse=", ")
      }
      #κρατάω μόνο τα στοιχεία που με ενδιαφέρουν, πολλές φορές τα μπέρδευε και τα έγραφε λάθος 
      header_items <- page_main %>% html_nodes("ul.ipc-inline-list--show-dividers li") %>% html_text()
      bad_words <- c("Cast & crew", "User reviews", "Trivia", "IMDbPro", "FAQ", "Videos", "Photos")
      header_items <- header_items[!header_items %in% bad_words]
      
      #χρήση regex για έτος, pg rating και διάρκεια ώστε να κάνει match
      year <- "NA"; pg_rating <- "NA"; duration <- "NA"
      y_match <- grep("^[0-9]{4}$", header_items, value=TRUE)
      if(length(y_match)>0) year <- y_match[1]
      cand <- header_items[header_items != year]
      p_match <- grep("^(G|PG|PG-13|R|NC-17|TV-[A-Z]+|Not Rated|Unrated|[0-9]+(\\+|[A-Z]?))$", cand, value=TRUE)
      if(length(p_match)>0) pg_rating <- p_match[1]
      d_match <- grep("[0-9]+h|[0-9]+m", header_items, value=TRUE)
      if(length(d_match)>0) duration <- d_match[1]
      # collecting βαθμολογίες και οικονομικά στοιχεία όπου υπάρχουν
      imdb_rating <- "NA"
      if (!is.null(json_data$aggregateRating$ratingValue)) {
        imdb_rating <- as.character(json_data$aggregateRating$ratingValue)
      } else {
        imdb_rating <- page_main %>% html_node("[data-testid='hero-rating-bar__aggregate-rating__score'] span") %>% html_text()
      }
      popularity <- page_main %>% html_node("[data-testid='hero-rating-bar__popularity__score']") %>% html_text()
      metascore <- page_main %>% html_node(".metacritic-score-box") %>% html_text()
      budget <- page_main %>% html_node("[data-testid='title-boxoffice-budget'] .ipc-metadata-list-item__list-content-item") %>% html_text()
      gross_world <- page_main %>% html_node("[data-testid='title-boxoffice-cumulativeworldwidegross'] .ipc-metadata-list-item__list-content-item") %>% html_text()
      gross_us <- page_main %>% html_node("[data-testid='title-boxoffice-grossdomestic'] .ipc-metadata-list-item__list-content-item") %>% html_text()
      opening_wk <- page_main %>% html_node("[data-testid='title-boxoffice-openingweekenddomestic'] .ipc-metadata-list-item__list-content-item") %>% html_text()
     
      #παίρνουμε τα counts και βρίσκουμε νέα ids για να γεμίσουμε την ουρά με recommendations
      review_counts <- page_main %>% html_nodes("ul[data-testid='reviewContent-all-reviews'] .ipc-inline-list__item") %>% html_text()
      user_reviews_count <- str_extract(review_counts[1], "[0-9.,K]+") 
      critic_reviews_count <- str_extract(review_counts[2], "[0-9.,K]+") 
      new_ids_found <- page_main %>% html_nodes("a") %>% html_attr("href") %>% str_extract("tt[0-9]+") %>% na.omit() %>% unique()
      
      #σελίδα των ratings για να δούμε την κατανομή των αστεριών αναλυτικά
      ratings_link <- paste0("https://www.imdb.com/title/", current_id, "/ratings/")
      page_ratings <- get_page_content(ratings_link)
      rating_dist <- "NA"
      
      if(!is.null(page_ratings)) {
        raw_html <- as.character(page_ratings)
        #search for exact pairs για να είμαστε σίγουροι ότι είναι σωστά
        matches_A <- str_match_all(raw_html, '"rating":([0-9]+),"voteCount":([0-9]+)')[[1]]
        matches_B <- str_match_all(raw_html, '"voteCount":([0-9]+),"rating":([0-9]+)')[[1]]
        star_counts <- setNames(rep("0", 10), 1:10)
        
        #γέμισμα πίνακα με τις ψήφους
        if(nrow(matches_A) > 0) {
          for(i in 1:nrow(matches_A)) {
            r <- matches_A[i, 2]; c <- matches_A[i, 3]
            if(r %in% names(star_counts)) star_counts[r] <- c
          }
        }
        if(nrow(matches_B) > 0) {
          for(i in 1:nrow(matches_B)) {
            c <- matches_B[i, 2]; r <- matches_B[i, 3]
            if(r %in% names(star_counts)) star_counts[r] <- c
          }
        }
        
        if(any(star_counts != "0")) {
          rating_dist <- paste0(
            "10*: ", star_counts["10"], " | ", "9*: ", star_counts["9"], " | ",
            "8*: ", star_counts["8"], " | ", "7*: ", star_counts["7"], " | ",
            "6*: ", star_counts["6"], " | ", "5*: ", star_counts["5"], " | ",
            "4*: ", star_counts["4"], " | ", "3*: ", star_counts["3"], " | ",
            "2*: ", star_counts["2"], " | ", "1*: ", star_counts["1"]
          )
        }
      }
      #reviews για κείμενα και περιλήψεις
      reviews_link <- paste0("https://www.imdb.com/title/", current_id, "/reviews")
      page_reviews <- get_page_content(reviews_link)
      
      review_topics <- "NA" 
      review_summary_text <- "NA"
      review_samples <- "NA"
      
      if(!is.null(page_reviews)) {
        #ελεγχος αν υπάρχει AI περίληψη
        summary_div <- page_reviews %>% html_nodes(".ipc-html-content-inner-div") %>% html_text()
        if(length(summary_div) > 0) review_summary_text <- summary_div[1]
        #αν με το JSON είναι άδειο, σκουπίζει με αυτο τον τρόπο όλα τα κείμενα από το HTML
        if (!is.null(json_data$review)) {
          j_reviews <- json_data$review$reviewBody
          if(!is.null(j_reviews)) review_samples <- paste(head(j_reviews, 10), collapse=" ||| ")
        }

        if (review_samples == "NA") {
          review_containers <- page_reviews %>% html_nodes(".lister-item-content") 
          texts_found <- c()
          if (length(review_containers) > 0) {
            for (node in review_containers) {
              t_val <- node %>% html_node(".content .text") %>% html_text() %>% str_trim()
              if (is.na(t_val) || nchar(t_val) < 5) {
                full_text <- node %>% html_text()
                parts <- unlist(strsplit(full_text, "\n"))
                t_val <- parts[which.max(nchar(parts))] %>% str_trim()
              }
              if (!is.na(t_val) && nchar(t_val) > 10) texts_found <- c(texts_found, t_val)
            }
          }
          if(length(texts_found) > 0) {
            limit <- min(10, length(texts_found))
            review_samples <- paste(texts_found[1:limit], collapse = " ||| ")
          }
        }
      }
      #φτιάχνουμε το τελικό dataframe
      record <- data.frame(
        id = current_id, title = title, year = ifelse(is.na(year), "NA", year),
        pg_rating = ifelse(is.na(pg_rating), "NA", pg_rating), duration = ifelse(is.na(duration), "NA", duration),
        imdb_rating = ifelse(is.na(imdb_rating), "NA", imdb_rating), popularity = ifelse(is.na(popularity), "NA", popularity),
        genres = ifelse(is.na(genres) || genres == "", "NA", genres), metascore = ifelse(is.na(metascore), "NA", metascore),
        budget = ifelse(is.na(budget), "NA", budget), gross_worldwide = ifelse(is.na(gross_world), "NA", gross_world),
        gross_us_canada = ifelse(is.na(gross_us), "NA", gross_us), opening_weekend = ifelse(is.na(opening_wk), "NA", opening_wk),
        user_reviews_count = ifelse(is.na(user_reviews_count), "NA", user_reviews_count),
        critic_reviews_count = ifelse(is.na(critic_reviews_count), "NA", critic_reviews_count),
        rating_distribution = rating_dist, review_topics = review_topics,
        review_summary = review_summary_text, review_samples = review_samples,
        stringsAsFactors = FALSE
      )
      
    }
  }, error = function(e) { print(paste("Error:", e$message)) }) #in case για καποιο error
  
  if (!is.null(record)) { 
    all_movies_data[[current_id]] <- record 
  }
  
  #βγάζουμε αυτά που είδαμε, βάζουμε τα καινούργια και ανανεώνουμε το queue
  new_ids_to_add <- setdiff(new_ids_found, visited_ids)
  new_ids_to_add <- setdiff(new_ids_to_add, queue_ids)
  queue_ids <- c(queue_ids, new_ids_to_add)
  visited_ids <- c(visited_ids, current_id)
  #aυτόματη αποθήκευση/20 ταινίες just in case
  if (length(all_movies_data) %% 20 == 0 && length(all_movies_data) > 0) {
    save(all_movies_data, queue_ids, visited_ids, file = "IMDB_Full_Data_Backup.RData")
    print(paste(" Auto-Saved Backup at", length(all_movies_data), "movies."))
  }
  
  Sys.sleep(1.5) #pause για να μην φάμε ban
}
#αποθήκευση
final_df <- bind_rows(all_movies_data)
save(final_df, file = "IMDB_Final_5000_Movies.RData")
write.csv(final_df, "IMDB_Final_5000_Movies.csv", row.names = FALSE)