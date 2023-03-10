# Private Federated Learning with Domain Adaption
Paper by Daniel Peterson, Pallika Kanami, and Virendra Marathe  
Presentation by Olav Pelzer  
Summary by Simon van Hus (6147879)

---

This paper explores a modification to the federated learning model dubbed "mixed expert learning".
In federated learning (FL) a central server keeps track of a global model and shares it with a client base.
These clients than use their own data to train the model into their own local model.
Then, the clients send their updated local models back to the global server, which recombines them into the new global model.
Clients with more training data are weighted heavier in the recombination of the local models into the new global model.
While FL allows for sharing the workload of training complex models, it might run into heavyweight clients (those with the most data) controlling the direction of the training, and client data that might not be quite compatible with other client's data.

To fix this the authors look at transfer learning as well, where a pretrained model is modified to work on a new set of data.
The authors are specifically interested transforming the new domain so it aligns with the old training domain, so the old model can be used on the new data.
This is called domain adaption.

The paper actually focusses on combining both federated learning and domain adaption.
Their method, "mixture of experts", mixes the global model output with the local model output using a gating function.
The goal here is to train a regression model to decide when to use the global model and when to use the local model. (This is actually mixed with a gating function $\alpha:[0,1]$.)

When they tested this model on two clients (with added noise), the results were quite promising.
However these clients were just linear functions, and contained no real data, so these results might not be too representative.

<br/>
<img
    src="./memes/mixed experts.png"
    style="width: 30%; margin-left: 35%;"
    title="Two buttons meme, with both pressed, one labeled 'federated learning' and the other 'domain adaption'."
    alt="Two buttons meme, with both pressed, one labeled 'federated learning' and the other 'domain adaption'." />