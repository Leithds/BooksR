# Load libs
library(dplyr)
library(stringr)

# Load dataset
books <- read.csv("books.csv", stringsAsFactors = FALSE)

# Clean num_pages
books$num_pages[!grepl("^[0-9]+$", books$num_pages)] <- NA
books$num_pages <- as.numeric(books$num_pages)

# find books similar to
recommend_books <- function(dataset) {
  repeat {
    # Get user input
    liked_book <- readline(prompt = "Enter the title of a book you liked (or leave blank to skip): ")
    user_rating <- readline(prompt = "Enter a minimum average rating (e.g., 4.0): ")
    max_pages <- readline(prompt = "Enter a maximum number of pages allowed (leave blank for no limit): ")

    # Validate and clean inputs
    liked_book <- str_trim(liked_book)  # Trim any extra spaces

    # Validate user_rating input
    if (user_rating == "") {
      user_rating <- NA  # Skip the rating filter if left blank
    } else {
      user_rating <- as.numeric(user_rating)
      if (is.na(user_rating) || user_rating <= 0) {
        print("Invalid rating input. Please enter a positive number for rating.")
        next  # Go back to asking for input
      }
    }

    # Handle max_pages input correctly
    if (max_pages == "") {
      max_pages <- Inf  # No page limit if left blank
    } else {
      max_pages <- as.numeric(max_pages)
      if (is.na(max_pages) || max_pages <= 0) {
        print("Invalid page count input. Please enter a positive number for pages.")
        next  # Go back to asking for input
      }
    }

    # Validate preferred language input
    languages_available <- unique(dataset$language_code)
    language_filter <- readline(prompt = "Enter preferred language (leave blank for English): ")

    if (language_filter != "") {
      # Loop until a valid language is entered
      while (!(language_filter %in% languages_available)) {
        print("Invalid language. Please enter a valid language from the dataset.")
        language_filter <- readline(prompt = "Enter preferred language (leave blank for English): ")
      }
      dataset <- dataset %>% filter(language_code == language_filter)
    } else {
      # Default to English if no input provided
      dataset <- dataset %>% filter(language_code == "eng")
    }

    # Filter logic based on liked book
    if (liked_book != "") {
      # Find books similar to the one the user liked
      similar_books <- dataset %>% 
        filter(str_detect(title, regex(liked_book, ignore_case = TRUE)))
      
      if (nrow(similar_books) > 0) {
        # User input matches at least one book in the dataset
        selected_book <- similar_books[1, ]
        recommended_books <- dataset %>%
          filter(authors == selected_book$authors & 
                   average_rating >= user_rating & 
                   num_pages <= max_pages)
      } else {
        # No match based on the title
        print("Sorry, no recommendations based on that book. Here are some general recommendations!")
        recommended_books <- dataset %>%
          filter(average_rating >= user_rating & num_pages <= max_pages)
      }
    } else {
      recommended_books <- dataset %>%
        filter(average_rating >= user_rating & num_pages <= max_pages)
    }

    # Check if any recommendations are found
    if (nrow(recommended_books) > 0) {
      print("Books you might enjoy:")
      print(recommended_books %>%
              select(title, authors, average_rating, num_pages) %>%
              arrange(desc(average_rating)) %>%
              head(10))
      
      # Output recommendations to a file
      output_file <- "book_recommendations.txt"
      write.table(recommended_books %>%
                    select(title, authors, average_rating, num_pages),
                  file = output_file, 
                  row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
      print(paste("Recommendations have been saved to", output_file))
    } else {
      print("No books found for your preferences. Try adjusting your filters! (or maybe just ask someone)")
    }

    # Repeat loop
    more_recommendations <- readline(prompt = "Would you like more recommendations? (yes/no): ")
    if (tolower(more_recommendations) != "yes") {
      break
    }
  }
}

recommend_books(books)