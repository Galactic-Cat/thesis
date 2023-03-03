# BO as Assistant: Using Bayesian Optimization for Asynchronously Generating Design Suggestions
Paper by Yuki Koyama and Masataka Goto  
Presentation by Allison Lo  
Summary by Simon van Hus (6147879)

---

In this paper, the authors describe a system to use the input of sliders of design software (image editors, 3d modellers, etc.) as input for a Preferred Bayesian Optimization (PBO) system that can provide suggestions.
The main idea is that by using the movement of the sliders as datapoints, the PBO system can predict the design goal of the user and provide suggestions in that direction.

The authors point out that other similar AI systems that try to fully automate the design process might not be flexible enough for certain design goals.
These fully autonomous AI systems also fail to use the domain knowledge of the designers.
By using PBO as a mere assistant that learns from the designers input, the designer is free to use as little or as much assistant as they see fit, and their domain knowledge is respected.

The framework generates three suggestions at based on user slider input.
These suggestions can then be blended (using more sliders) with the current work, or three new suggestions can be generated.

The authors note a couple of assumptions they make about the designing process.
Namely, that the parameters are of similar types, the design process provides a real-time preview, the designing is done with a goal in mind, the parameters change continuously, the number of parameters lies approximately between two and twenty, and the parameters influence each other (and can thus be meaningfully combined in to a single multi-dimensional datapoint.)

The authors also note that they have explored multiple data points collection strategies.
First off, they take the initial state as the only undesired data point, and every change made from there as desired data points.
This tended to produce extreme results as it basically created a one-sided barrier on the preference space.  
The second strategy collected every previous slider change as a non-desired data point, this lead to a quick accumulation of data points noticeably slowing the PBO system.
This also lead to an overly restricted search space.  
Finally, the best method the authors claimed to have found, was to record all the turning points on the sliders.
These points were where the designer moved the slider in back in the opposite direction they were originally moving it in.
Recording these datapoints as undesirable lead to the most reasonable estimates according to the authors.

For future work the authors rightfully note that the designer might not always make the design better, might not have a design goal in mind, or might change their preference on the fly.
To combat this the authors note that an option to start the PBO framework fresh by deleting all previous data points could be implemented; or they weigh later data points stronger and have older data points decay over time.

<br />
<img
    src="./memes/sliders everywhere.png"
    style="width: 50%; margin-left: 25%;"
    title="Woody and Buzz meme, with top text 'Sliders' and bottom text 'Siders everywhere'."
    alt="Woody and Buzz meme, with top text 'Sliders' and bottom text 'Siders everywhere'." />