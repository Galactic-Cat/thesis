# A MultiObjective Optimization Approach for Integrated Timetabling and Vehicle Scheduling with Uncertainty
Paper by Yindong Shen, Wenliang Xie, and Jingpeng Li  
Presentation by Glenn Hamers  
Summary by Simon van Hus (6147879)

---

In this paper, the authors set out to build an integrated solution to the timetabling problem (TTP) and the vehicles scheduling problem (VSP), for public transport scheduling.
Scheduling public transport requires a multitude of processes to be optimized:
- First, routes are designed
- Second, the timetabling of routes
- Third, the scheduling of vehicles
- And finally, the scheduling of crews to the vehicles
Optimizing these things sequentially leads to each following step becoming harder to optimize, due to the extra restrictions posed by the previous step(s).

The integrated system this paper builds creates a timetable, and scheduling for vehicles and crew.
The routes are static, and decided on beforehand.
The authors manage to model the problem as a mixed-integer linear programming (MILP) model, with only eleven constraints, pertaining to:
- Passenger travel optimization (duration of the trip and wait times)
- Vehicle scheduling optimization
- Incompatibility between route linking (vehicles moving from route to route)

Interestingly enough, they use a self-adjusting genetic algorithm to generate a timeframes to optimize, the exact merits of which are little unclear to me.
Then they use large neighbourhood search to optimize these timeframes.
They iterate this process until a maximum number of iterations is reached.

The authors test their system on a real-world scenario, and find that their integrated model can find more efficient routes than a sequential optimization.

<img
    src="./memes/travel ghosts.png"
    style="width: 50%; margin-left: 25%" />