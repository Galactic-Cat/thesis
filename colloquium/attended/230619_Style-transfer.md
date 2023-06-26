# Style Transfer from Non-Parallel Text by Cross-Alignment
Paper by Tianxiao Shen, Tao Lei, Regina Barzilay, and Tommi Jaakkola  
Presentation by Ziyuan Wang  
Summary by Simon van Hus (6147879)

---

This paper looks at style-alignment in text, which is the latent presence of writing style regardless of content in a corpus.
The authors propose a system of 'cross-alignment' which would be able to extract content from style, or insert content using style when faced with two corpuses of text.

They do this on three tasks, first of which is sentiment modification.
Sentiment modification, as the name suggests, is a task of altering the sentiment in a sentence while minimally altering the content of it.
This paper's model does a decent job at this tasks, however it is outperformed by the model they compare it to.
The authors pose that this is probably because the other model uses an actual sentiment classifier, while they do not.

The second task is word substitution decipherment, where sentences are encrypted by substituting words, and the model is tasked to recover the original meaning.
While the task is trivial when the model is provided with parallel data, it does not perform very well when it is not.

The third task is word order recovery, where as the name suggests, the model is tasked with recovering the correct order of a scrambled sentence.
This task is much harder, and this is also reflected in the results found in the paper.

All in all, the method provided in the paper is fairly novel, and the authors seem happy with their results.