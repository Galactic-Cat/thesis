# Dataset Shift Quantification for Credit Card Fraud Detection
Paper by Yvas Lucas et al.  
Presentation by Floor Kouwenberg  
Summary by Simon van Hus (6147879)

---

This paper talks about how dataset shift might be problematic for detecting credit card fraud using data mining and machine learning techniques.
As behaviours of fraudsters changes, so does might the data.

The authors quantify the day-to-day change in spending behaviour by using a forest classifier.
They then run the classifier on each of the 92 days in their dataset.
Then, they use MCC scoring, to measure how similar any two days in the dataset are, resulting in a 92x92 matrix of similarity.

The matrix shows a clear pattern, mostly of weekdays and weekends, but also of two weeks of vacation.
The authors use a global clustering algorithm to create four clusters found in the data.
Then they enhance the fraud detection classifier by providing the cluster as a feature.
Finally, they compared the fraud detection classifier with and without the cluster feature, and found that the classifier with the cluster feature performed $2.5\%$ better.

<br />
<img
    src="./memes/tiny improvement.png"
    style="width: 55%; margin-left: 22.5%;"
    title="Doctor Who and assistant talking about wether 2.5% is a lot, it is not."
    alt="Doctor Who and assistant talking about wether 2.5% is a lot, it is not." />