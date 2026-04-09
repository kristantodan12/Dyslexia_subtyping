library(tm)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(ggplot2)

# Run the helper script to get the dataframe
# This assumes the working directory is the project root
source("Scripts/helper.r")



#######################################################################
###### Plotting a word cloud of the specific aims of the papers #######
#######################################################################
# 2. Process the data
# Filter for unique papers based on Item_No and select Specific_aim
# We use distinct() to keep only the first occurrence of each Item_No
# This handles the case where one paper (e.g., ID13) has multiple rows/studies
cleaned_aims <- article_data %>%
  select(Item_No, Specific_aim) %>%
  distinct(Item_No, .keep_all = TRUE) %>%
  filter(!is.na(Specific_aim))

# 3. Text Cleaning
# Create a text corpus
corpus <- Corpus(VectorSource(cleaned_aims$Specific_aim))

# Convert to lowercase
corpus <- tm_map(corpus, content_transformer(tolower))

# Remove numbers
corpus <- tm_map(corpus, removeNumbers)

# Remove punctuation
corpus <- tm_map(corpus, removePunctuation)

# Remove English stopwords
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Remove extra white spaces
corpus <- tm_map(corpus, stripWhitespace)

# 4. Generate Word Cloud
# Create a Term-Document Matrix to count frequencies
dtm <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)
df_word_freqs <- data.frame(word = names(word_freqs), freq = word_freqs)

# Plot
set.seed(1234) # For reproducibility

save_dir <- "C:/Users/danie/Documents/Projects/Dyslexia_subtypes/Manuscript/Figures/From_script"
if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
png(file.path(save_dir, "word_cloud.png"), width = 8, height = 8, units = "in", res = 300)

wordcloud(words = df_word_freqs$word, 
          freq = df_word_freqs$freq, 
          min.freq = 2,           # Minimum frequency to be included
          max.words = 100,        # Maximum number of words to plot
          random.order = FALSE,   # Plot highest frequency words in center
          rot.per = 0.35,         # Percentage of vertical words
          colors = brewer.pal(8, "Dark2"))

dev.off()

# View top 10 words in the console
print(head(df_word_freqs, 10))


#######################################################################
###### Number of construct #######
#######################################################################
# Select columns related to constructs
construct_cols <- article_data %>% select(matches("^Para.*Construct$"))

# Unlist to get all values in a single vector
all_constructs <- unlist(construct_cols)

# Filter out NAs and "Not applicable" to get unique constructs
unique_constructs <- unique(all_constructs[!is.na(all_constructs) & all_constructs != "Not applicable"])

# Print the count and the list of unique constructs
cat("Number of unique constructs:", length(unique_constructs), "\n")
print(unique_constructs)

#######################################################################
###### Plotting the number of subtypes (Subtypes_N) #######
#######################################################################
# Filter for unique papers based on Item_No and select Subtypes_N
cleaned_subtypes <- article_data %>%
  distinct(Item_No, .keep_all = TRUE) %>%
  select(Subtypes_N) %>%
  filter(!is.na(Subtypes_N)) %>%
  mutate(Subtypes_N = as.numeric(as.character(Subtypes_N)))

# Plot
ggplot(cleaned_subtypes, aes(x = factor(Subtypes_N))) +
  geom_bar(fill = "forestgreen") +
  labs(title = "Distribution of Number of Subtypes", x = "Number of Subtypes", y = "Frequency") +
  theme_minimal(base_size = 20) +
  theme(
    axis.text = element_text(size = 40),
    axis.title = element_text(size = 42),
    plot.title = element_text(size = 45)
  )
