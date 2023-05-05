# Temporal graph networks for Deep Learning on Dynamic Graphs
Paper by Emanuele Rossi, Ben Chamberlain, Fabrizio Frasca, Davide Eynard, Federico Monti, Michael Bronstein  
Presentation by Yunming Hui  
Summary by Simon van Hus (6147879)

---

This paper presents a new model for deep learning on graphs: Temporal Graph Networks (TGNs).
The model is especially made for dynamic graphs that evolve over time, like those representing social media (the authors are all Twitter employees.)
They encode the data of the graph as a sequence of time-stamped events, where each timestamp gets an embedding of the graph at that time.
They also provide node memory in this embedding, which can store events in the network.
The complete embedding is then used to calculate the probability of each edge given the timestamp, which can then be used to get a loss-score, and train a neural network.

The authors state that the model can be used to train over a variety of dynamic graph problems.
They test the models performance on a dynamic node classification benchmark, where it outperforms the competition.
Through another benchmark they also show that, in the right configuration, the model can also train fairly quickly, without loss of accuracy.

<img
    src="./memes/time travel.jpeg"
    style="width: 40%; margin-left: 30%" />