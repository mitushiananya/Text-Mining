library("NLP")
library("tm")
library("RColorBrewer")
library("wordcloud")
library("wordcloud2")

# Read the text file from local machine 
# choose the file
text <- readLines(file.choose())
# Load the data as a corpus
TextLoad <- Corpus(VectorSource(text))

# Cleaning data
# 1. convert the text to lower case
textlower <- tolower(TextLoad)
head(textlower)

# 2. Remove punctuation from the text
# pattern=  “\\W’’ removes punctuation
textpunctuation <- gsub(pattern = "\\W", replace = " " ,textlower)
head(textpunctuation)

# 3. Remove digits from the text
textdigits <- gsub(pattern = "\\d", replace = " ", textpunctuation)
head(textdigits)


stopwords()

# 4. Remove stop words

textstopwords <- removeWords(textdigits,words = c(stopwords(),"ai","â"))
head(textstopwords)

# 5. Remove single letters
textsingleletters  <- gsub(pattern = "\\b[A-z]\\b{1}", replace = " ", textstopwords )
head(textsingleletters)

# 6. Remove white spaces
textwhitespaces <- stripWhitespace(textsingleletters)
head(textwhitespaces)

# Word Frequency
# splitwords
textsplitwords <- strsplit(textwhitespaces, " ")
head(textsplitwords)

# Create a term-document matrix
TextLoad_dtm <- TermDocumentMatrix(TextLoad)
dtm_m <- as.matrix(TextLoad_dtm)
# Sort by decreasing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d, 5)

# Plot the most frequent words
barplot(dtm_d[1:5,]$freq, las = 2, names.arg = dtm_d[1:5,]$word,
        col ="darkblue", main ="Top 5 most frequent words",
        ylab = "Word frequencies")

word_cloud <- unlist(textsplitwords)
wordcloud(word_cloud)

wordcloud2(dtm_d, color = "random-light", backgroundColor = "white")
