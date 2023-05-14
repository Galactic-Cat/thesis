# Bidirectional Encoder Representation from Transformers (BERT)
Paper by Jacob Devlin, Ming-Wei Chang, Kenton Lee, and Kristina Toutanova  
Presentation by Yuqi Liu  
Summary by Simon van Hus (6147879)

---

This paper introduces a language representation model called "Bidirectional Encoder Representation from Transformers" (BERT).
(They named it BERT on purpose, because there already was a language pretraining method called ELMo.)
The main idea behind BERT is bidirectionality, where instead of reading an input sentence left-to-right or right-to-left, using transformers it reads in both directions at once.
This means that BERT can be used on a wider variety of problems than either left-to-right or right-to-left only models.
The authors then train this model on the Masked Language Model (MLM) task, which "hides" a word in a sentence, which the model should then find based on context.
This task is useful for learning about word meaning, as it can be done unsupervised.
They also pretrain BERT on the Next Sentence Prediction (NSP) task, where the model should judge whether or not two sentences can be placed subsequently; this can also be done unsupervised.

Of course, as a language representation model, BERT does not do anything on its own.
Rather it can be prepended to another AI model and provide some representation of a sentence or word in a multidimensional relational space (so that words with similar meanings are similar).
This then means that those models do not need to learn the meaning of sentences or words, but only need to train on the BERT representation.

<br />
<img
    src="./memes/bert.webp"
    style="width:30%;margin-left:35%;" />