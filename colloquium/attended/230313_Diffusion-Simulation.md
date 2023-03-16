# Simulation of reaction diffusion processes over biologically relevant size and time scales using multi-GPU workstations
Paper by Michael Hallock, John Stone, Elijah Roberts, Corey Fry, and Zaida Luthey-Schulten  
Presentation by Gerard van Schie  
Summary by Simon van Hus (6147879)

---

This paper explains how simulations of cellular diffusion processes are computationally heavy.
These computations, which can be executed fairly efficiently on GPU architectures, would be handled more efficiently if we divide them over multiple GPUs.
They do this by dividing the simulated cells in equally sized blocks and distribute these blocks over the available GPUs.
To make sure these blocks don't desynchronize from each other, they are made to slightly overlap with each other in areas dubbed "halos".
These halos are then synchronized between the GPUs, so the blocks stay in sync.
To hide and minimize latency during these synchronizations, they are performed on a separate asynchronous data-stream.

The authors note that dividing the simulated cell into spatially equal parts doesn't actually guarantee an equal separation of the work load.
Especially for diffusion simulations dividing the simulated cell into spatially equal parts practically guarantees an uneven work load distribution.
So instead, the authors lay out an dynamic load balancing algorithm.
By changing the size of the areas simulated by the different GPUs depending on the work load in each area, this algorithm tries to distribute work load evenly among the GPUs (with areas with heavy work loads being smaller).
The work load is tracked by tracking the time it takes for each GPU to compute their calculation, and then trying to average those out by adjusting the sizes of their areas.

Results in the paper indicate that the load-balancing algorithm does work as expected, and that more and faster GPUs generally outperform less or slower GPUs.
The authors also note that as long as GPUs can handle the computational load without delay by themselves, adding additional GPUs actually only slows down the process.
So, additional GPUs only work if they allow for meaningful parallelism.
(This is also because the paper assumes an older NUMA architecture in which GPUs might need to communicate over the CPU, which is much slower than direct communication between GPUs.)

<br />
<img
    src="./memes/multiple gpus.png"
    style="width: 35%; margin-left: 32.5%;"
    title="Matrix's morpheus with top-text 'What if I told you' and bottom text 'it is a simulation on multiple GPUs'"
    alt="Matrix's morpheus with top-text 'What if I told you' and bottom text 'it is a simulation on multiple GPUs'" />