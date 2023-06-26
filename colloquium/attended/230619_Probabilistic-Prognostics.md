# Novel Metrics to Evaluate Probabilistic Remaining Useful Life Prognostics with Applications to Turbofan Engines
Paper by Ingeborg de Pater and Mihaela Mitici  
Presentation by Leo Jenneskens  
Summary by Simon van Hus (6147879)

---

Remaining Useful Life (RUL) prognostics is a field of predicting how long all kinds of things will still remain functional.
RUL prognostics have become important, because it is more profitable to only replace things near the end of their lifetime, than on a set schedule, or when they actually break.
In this paper, the authors look at RUL prognostics for turbofans.

The authors note that methods like the root mean square error and mean absolute error don't work very well for RUL prognostics problems.
Instead, the paper introduces a convolutional neural network to predict the RUL of a turbofan based on four metrics and its operational history.
Of course, there is a certain uncertainty when it comes to the actual condition of the fan, or about any defects that might arise that shorten the lifespan of the fan.
To account of this without explicitly modelling this uncertainty, the authors implement Monte Carlo dropout, which can model this as well.
Monte Carlo dropout sets the weight for random nodes in the network to zero, meaning they do not provide any signal to the rest of the network (dropping them out).
This effectively introduces noise into the neural network, which is used to model uncertainty about the future.

The authors finally show through some experiments that the predictions are well-fitted to real RUL prognoses.