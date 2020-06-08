
# Text Length distribution between real and fake news TEXT using histogram #

library(ggplot2)
# Convert Factor into Character vector to get Text Length #
char_data <- as.character(all_data$text)
all_data$TextLength <- nchar(char_data)
summary(all_data$TextLength)

# Visualise the results using plot #
ggplot(all_data, aes(x = TextLength, fill = class)) + theme_bw() +
  geom_histogram(binwidth = 30) +
  labs(y = "Text Count", x = "Length of Text",
       title = "Distribution of Text Lengths with class labels")

# Look at Summary Data for both Fake and True news datasets #
char_fake <- as.character(fake_news$text)
fake_news$TextLength <- nchar(char_fake)
summary(fake_news$TextLength)

char_true <- as.character(true_news$text)
true_news$TextLength <- nchar(char_true)
summary(true_news$TextLength)

# Text Length distribution between real and fake news TITLE using histogram #

char_title <- as.character(all_data$title)
all_data$TitleLength <- nchar(char_title)
summary(all_data$TitleLength)
ggplot(all_data, aes(x = TitleLength, fill = class)) + theme_bw() +
  geom_histogram(binwidth = 30) +
  labs(y = "Text Count", x = "Length of Text",
       title = "Distribution of Text Lengths with class labels")
