
library(syuzhet)

#--------------

get_sentiment("I love apples")
get_sentiment("I hate apples")
get_sentiment("I hate apples", method="nrc", lang="english")

t <- c("In my humble opinion Mr Biden is much better President of the United Stats than Mr Trump.",
       "Others say Mr Trump was a big a stain in the history of the US.")
get_sentiment(t)

# -------------


my_example_text <- "I begin this story with a neutral statement.  
  Basically this is a very silly test.  
  You are testing the Syuzhet package using short, inane sentences.  
  I am actually very happy today. 
  I have finally finished writing this package.  
  Tomorrow I will be very sad. 
  I won't have anything left to do. 
  I might get angry and decide to do something horrible.  
  I might destroy the entire package and start from scratch.  
  Then again, I might find it satisfying to have completed my first R package. 
  Honestly this use of the Fourier transformation is really quite elegant.  
  You might even say it's beautiful!"

sv <- get_sentences(my_example_text)
str(sv)
head(sv)

pwv <- get_tokens(sv, pattern = "\\W")

syuzhet_vector <- get_sentiment(pwv, method="syuzhet")
syuzhet_vector

bing_vector <- get_sentiment(pwv, method = "bing")
bing_vector

afinn_vector <- get_sentiment(pwv, method = "afinn")
afinn_vector

nrc_vector <- get_sentiment(pwv, method = "nrc", lang = "english")
nrc_vector


# Because the different methods use different scales,
# it may be more useful to compare them using R's built in sign function.
# The sign function converts all positive number to 1,
# all negative numbers to -1 and all zeros remain 0.

rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector)),
  sign(head(nrc_vector))
)

# get a measure of the overall sentiment valence in the text 
sum(syuzhet_vector)

# let's check what is the central tendency
mean(syuzhet_vector)

# let's see the other parameters
summary(syuzhet_vector)
summary(bing_vector)
summary(afinn_vector)
summary(nrc_vector)


# Now, let's represent the passage of time from the beginning
# to the end of the text, and the y-axis measures the degrees of
# positive and negative sentiment.

sv_sentiment <- get_sentiment(sv)
plot(
  sv_sentiment, 
  type="l", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)


# Using a real novel (James Joyce, "A portrait of the artist": +300 pages)

path_to_text_file <- system.file("extdata", "portrait.txt",package = "syuzhet")
joyces_portrait <- get_text_as_string(path_to_text_file)
pv <- get_sentences(joyces_portrait)
senti <- get_sentiment(pv)

plot(
  senti, 
  type="h", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

# enhancing the visualization using bins
percent_vals <- get_percentage_values(senti, bins = 20)
plot(
  percent_vals, 
  type="l", 
  main="Joyce's Portrait Using Percentage-Based Means", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence", 
  col="red"
)


# ---- get_dct_transform ------------

# trying a smoother transition
dct_values <- get_dct_transform(
  senti, 
  low_pass_size = 5,   # default
  x_reverse_len = 100, # default
  scale_vals = F,      # default
  scale_range = T      # scale the values from -1 to +1
)

plot(
  dct_values, 
  type ="l", 
  main ="Joyce's Portrait using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)

simple_plot(senti)

# ------ Another example --------------

# The novel "Madame Bovary" (1856)
path_to_text_file <- system.file("extdata", "bovary.txt", package = "syuzhet")
bovary <- get_text_as_string(path_to_text_file)
vb <- get_sentences(bovary)
bovary_sentiment <- get_sentiment(vb)

plot(
  bovary_sentiment, 
  type ="l", 
  main ="Joyce's Portrait using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)

simple_plot(bovary_sentiment)



# ------- Analyzing emotions ---------

# The get_nrc_sentiment implements Saif Mohammad's NRC Emotion lexicon.
# The NRC emotion lexicon is a list of words and their associations
# with eight emotions (anger, fear, anticipation, trust, surprise, sadness,
# joy, and disgust) and two sentiments (negative and positive)

# http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm


# -- Get NRC Sentiment

# The get_nrc_sentiment function returns a data frame in which
# each row represents a sentence from the original file.
# The columns include one for each emotion type was well as the
# positive or negative sentiment valence. 

nrc_data <- get_nrc_sentiment(sv)
nrc_data

# Now, let's check the emotions
anticipation_items <- which(nrc_data$anticipation > 0)
sv[anticipation_items]
anger_items <- which(nrc_data$anger > 0)
sv[anger_items]


# exploring the percentage of each emotion
barplot(
  sort(colSums(prop.table(nrc_data[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.8, 
  las = 1, 
  main = "Emotions in Sample text", xlab="Percentage"
)

# ---- Re-scaling the sentiments

# The rescale_x_2 function is handy for re-scaling values to a
# normalized x and y axis. This is useful for comparing two sentiment curves.
# Assume that we want to compare the shapes produced by applying a moving average
# to two different sentiment distributions.

# First we'll compute the raw sentiment values in
# 'Portrait of the Artist' and 'Madame Bovary'.

path_to_a_text_file <- system.file("extdata", "portrait.txt",package = "syuzhet")
joyces_portrait <- get_text_as_string(path_to_a_text_file)
pv <- get_sentences(joyces_portrait)
poa_values <- get_sentiment(pv, method="syuzhet")

path_to_a_text_file <- system.file("extdata", "bovary.txt", package = "syuzhet")
bovary <- get_text_as_string(path_to_a_text_file)
bv <- get_sentences(bovary)
bovary_values <- get_sentiment(bv)


# Now we calculate a moving average for each vector of raw values.
# We use a window size equal to 1/10 of the overall length of the vector.

pwdw <- round(length(poa_values)*0.1)
poa_rolled <- zoo::rollmean(poa_values, k=pwdw)
bwdw <- round(length(bovary_values)*0.1)
bov_rolled <- zoo::rollmean(bovary_values, k=bwdw)

# The resulting vectors are of different lengths: 4814 and 6248
# We can use rescale_x_2 to put them on the same scale.
# The function rescales input values to two scales (0 to 1 and -1 to 1)
# on the y-axis and also creates a scaled vector of x axis values from 0 to 1.

poa_list <- rescale_x_2(poa_rolled)
bov_list <- rescale_x_2(bov_rolled)

# We can then plot the two lines on the same graph
# even though they are of different lengths (because of new x scale)

plot(poa_list$x, 
     poa_list$z, 
     type="l", 
     col="blue", 
     xlab="Narrative Time", 
     ylab="Emotional Valence"
)
# overwrite with the Bovary line
lines(bov_list$x, bov_list$z, col="red")


# Though we have now managed to scale both the x axis and y axis
# so as to be able to plot them on the same graph,
# we still don't have vectors of the same length, which means that
# they cannot be easily compared mathematically. The time axis for
# Portrait of the Artist is 4814 units long and the time axis for
# Madame Bovary is 6248 units.

# It is possible to sample from these vectors.
# E.g., divide each vector into 100 samples and then plot those sampled points.
# The result is that each line is constructed out of 100 points on the x-axis.

poa_sample <- seq(1, length(poa_list$x), by=round(length(poa_list$x)/100))
bov_sample <- seq(1, length(bov_list$x), by=round(length(bov_list$x)/100))

plot(poa_list$x[poa_sample], 
     poa_list$z[poa_sample], 
     type="l", 
     col="blue",
     xlab="Narrative Time (sampled)", 
     ylab="Emotional Valence"
)
lines(bov_list$x[bov_sample], bov_list$z[bov_sample], col="red")



# With a equal number of values, in each vector, we can then apply a measure
# of distance or similarity, such as Euclidean distance of Pearson's Correlation.

# Euclidean (rows)
dist(rbind(poa_list$z[poa_sample], bov_list$z[bov_sample]))

# Correlation (columns)
cor(cbind(poa_list$z[poa_sample], bov_list$z[bov_sample]))


# ----- smoothing differently ----------------

# Some users may find this sort of normalization and sampling preferable
# to the alternative method provided by the get_dct_transform, which assumes
# that the main flow of the sentiment trajectory is found within the
# low frequency components of the transformed signal.
# In this example, we sampled from a set of values that have been smoothed
# using a moving average (which is a type of low-pass filter).
# Once could apply this same routine to values that have been smoothed
# using some other method, such as the 'loess smoother' that is implemented
# in the simple_plot function.

poa_x <- 1:length(poa_values)
poa_y <- poa_values
raw_poa <- loess(poa_y ~ poa_x, span=.5)
poa_line <- rescale(predict(raw_poa))

bov_x <- 1:length(bovary_values)
bov_y <- bovary_values
raw_bov <- loess(bov_y ~ bov_x, span=.5)
bov_line <- rescale(predict(raw_bov))

poa_sample <- seq(1, length(poa_line), by=round(length(poa_line)/100))
bov_sample <- seq(1, length(bov_line), by=round(length(bov_line)/100))

plot(poa_line[poa_sample], 
     type="l", 
     col="blue",
     xlab="Narrative Time (sampled)", 
     ylab="Emotional Valence"
)
lines(bov_line[bov_sample], col="red")



# -------------- Mixed messages ------------

# Sometimes we might want to identify areas of a text where there is
# emotional ambiguity. The mixed_messages() function offers one way of
# identifying sentences that seem to have contradicting language.
# This function calculates the "emotional entropy" of a string based on the
# amount of conflicting valence found in the sentence's words.

# Emotional entropy can be thought of as a measure of unpredictability and
# surprise based on the consistency or inconsistency of the emotional language
# in a given string. A string with conflicting emotional language may be said
# to express or contain a "mixed message."
# Here is an example that attempts to identify and plot areas in Madame Bovary
# that have the highest and lowest concentration of mixed messages.


path_to_a_text_file <- system.file("extdata", "bovary.txt", package = "syuzhet")
sample <- get_text_as_string(path_to_a_text_file)
sample_sents <- get_sentences(sample)
test <- lapply(sample_sents, mixed_messages)

entropes <- do.call(rbind, test)
out <- data.frame(entropes, sample_sents, stringsAsFactors = FALSE)

simple_plot(out$entropy,
            title = "Emotional Entropy in Madame Bovary",
            legend_pos = "top")


# Obs:
# From this graphic it appears that the first part of the novel has the highest
# concentration of emotionally ambiguous, or conflicting, language.

# We might also plot the metric entropy which normalizes the entropy
# values based on sentence lengths.

simple_plot(out$metric_entropy,title = "Metric Entropy in Madame Bovary",
            legend_pos = "bottom")


# This graph tells a slightly different story. It still shows the beginning
# of the novel to have high emotional entropy, but also identifies a second
# wave at around the 1800th sentence.
# If we want to look at the specific sentences, we can sort them using dplyr.
# Them let's just examine a few sentences

library(dplyr)
sorted <- arrange(out, desc(entropy)) %>%
  select(entropy, sample_sents)
sorted[7:10, ]

# Let's check the high metrics
metric_sorted <- arrange(out, desc(metric_entropy)) %>%
  select(metric_entropy, sample_sents)
metric_sorted[1:6,]
