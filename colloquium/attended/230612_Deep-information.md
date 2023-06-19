# Deep information propagation
Paper by Samuel S. Schoenholz, Justin Gilmer, Surya Ganguli, and Jascha Sohl-Dickstein  
Presentation by Julian Markus  
Summary by Simon van Hus (6147879)

---

This paper is about propagating signals through randomly instantiated neural networks.
The authors note that commonly used randomly instantiated neural networks may not be training (optimally) because the signal of the input neurons does not make a meaningful connection to the output neurons.
This is influenced by the settings of the hyperparameters of the neural network, the number of layers, and the activation functions used.
Similarly, the authors also note that for training through backpropagation, the signal of the gradient needs to be able to reach throughout the network as well.

The authors show their hypothesis by experimenting on multiple neural networks, varying the variance of the initial weights and a universal network scale parameter, and then compare accuracy between the measurements.
THis shows a correlation between the size of the network, the variance of the weights, and the accuracy of the model produced.
It also shows a kind of threshold for these variance and scale parameters after which the accuracy starts to rapidly break down.

In their conclusion, the authors note that it might be useful to find some pre-training scheme that perturb or instantiate the weights in such as a way that both the forward and backpropagation signals can successfully pass through the network.

<br />
<img src="./memes/neural network.png"
    style="width: 45%; margin-left: 27.5%" />