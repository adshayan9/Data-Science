# Load Sentimentr package for Sentiment Analysis capabilties #
library(sentimentr)

# Sentiment Analysis for Title #

# Clean/Tidy data by getting rid of special signs that affect sentiment analysis #
clean_data <- gsub("http.*","", random_data$title)

clean_data <- gsub("https.*","",clean_data)

clean_data <- gsub("#.*","",clean_data)

clean_data <- gsub("@.*","",clean_data)

# Turn the data frame into a vector so that sentences can be seperated easier and sentiment analysis can take place #
word.df <- as.vector(clean_data)

sent.value <- get_sentences(word.df)
sent_test <- sentiment_by(sent.value)

# Tidy up and organise the data #
clean_sent <- sent_test[,c(2,4)]
colnames(clean_sent) <- c("Title_word_count", "Title_Sentiment")
randomwith_sent <- cbind(random_data, clean_sent)

# Sentiment Analysis for Text #
clean_text <- gsub("http.*","", random_data$text)

clean_text <- gsub("https.*","",clean_text)

clean_text <- gsub("#.*","",clean_text)

clean_text <- gsub("@.*","",clean_text)

text.df <- as.vector(clean_text)

sent.text <- get_sentences(text.df)
sent_textwvalues <- sentiment_by(sent.text)

clean_textsent <- sent_textwvalues[,c(2,4)]
colnames(clean_textsent) <- c("Text_word_count", "Text_Sentiment")
randomwith_sent <- cbind(randomwith_sent, clean_textsent)

# Write into a csv file to save time and keep a copy of work #
write.csv(randomwith_sent, "Textanalytics_data.csv")
