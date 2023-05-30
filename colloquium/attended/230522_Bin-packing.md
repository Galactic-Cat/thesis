# Online bin packing problem with buffer and bounded size revisited
Paper by Minghui Zhang, Xin Han, Yan Lan, and Hing-Fung Ting  
Presentation by Zhadyra Khattar  
Summary by Simon van Hus (6147879)

---

This paper looks at the online bin packing problem, where we want to pack items into bins, such that we need the least amount of bins possible.
The variant this paper looks at comes with bounded item size, such that the size of an item is within the range $(\alpha,1/2]$, where $0\leq\alpha\lt1/2$.
It also has a buffer with some constant size, we can delay packing items by temporarily in the buffer if there is room for the item in the buffer.
However, the items remaining in the buffer after all inputs have been presented need to be packed as well of course.
Items that are packed into a bin, can no longer be unpacked or moved.

The authors propose a pair of variant algorithms for this problem.
More importantly, they do some analysis that shows how their algorithm are 1.4243-competitive, which is an improvement from the previously proposed 1.4444-competetive algorithm.
Through their analysis they also show that lower bound for the competitive ratio is 1.4230, also an improvement from the previously found 1.3333.

<br />
<img src="./memes/packing.png"
     style="width: 40%; margin-left: 30%;" />