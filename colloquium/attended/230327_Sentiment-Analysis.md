# Toward Tag-free Aspect Based Sentiment Analysis: A Multiple Attention Network Approach
Paper by Yao Qiang, Xin Li, and Dongxiao Zhu  
Presentation by Liang Feng  
Summary by Simon van Hus (6147879)

---

In this paper, the authors set out to build a novel sentiment analysis model.
The main problem with sentiment analysis implementations, according to the authors, is the requirement for large tagged databases to train the neural network model.
Their model will provide aspect-based sentiment analysis on TripAdvisor reviews.
The trick here is that reviews are already tagged: the rating a reviewer leaves gives away the sentiment of the review.
Even better, reviews on TripAdvisor offer ratings per review-aspect.
For restaurants this may be: food, atmosphere, price, etc.

The authors first lemmatize and tokenize the reviews of sufficient length, using position embedding to make sure it is clear which words reference which aspects.
Then they feed this to a neural network which provides the aspect sentiment analysis.
They use cross-entropy loss minimization between the target sentiment and the predicted sentiment as the fitness measure of the network.

The final result is a neural network that performs well when compared to other state-of-the-art (in 2020) aspect-bases sentiment analysis systems.

<br />
<img 
    src="./memes/star ratings.png"
    style="width: 40%; margin-left: 30%;"
    title="XKCD 1098: Star ratings"
    alt="XKCD 1098: Star ratings" />