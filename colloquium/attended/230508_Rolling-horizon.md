# A rolling horizon approach for a multi-stage stochastic fixed-charge transportation problem with transshipment
Paper by Rossana Cavagnini, Luca Bertazzi, and Francesca Maggioni  
Presentation by Martijn Goes  
Summary by Simon van Hus (6147879)

---

This paper explores a stochastic transportation problem.
Much like in real life, the authors model a multi-stage transportation network, where they allow transshipment.
Transshipment is where retailers (or clients of the logistics company, the final link on the transportation chain) can ship surplus goods to other retailers with a shortage.
They only do this if there is a surplus after their own demand has been satisfied.
Transshipment can be more efficient and timely than sending more product from a central warehouse or a local depot.

The authors of this paper implement their problem as a multi-stage mixed integer stochastic programming problem, where the objective is to minimize the total expected cost.
They find that this problem is not only NP-hard, but also hard to solve for state-of-the-art solvers, especially when more than three layers are introduced (or multiple dimensions).
Instead the authors use a rolling horizon method, where precise calculations are only made up until some horizon after which a heuristic is used; this horizon is then extended with every iteration.

The paper finds that with the rolling horizon heuristic computation time is greatly reduced, while still maintaining a decent quality of solutions.
The authors also note that after finding the a solution with the horizon heuristic, an improved solution can be obtained by using it as the starting solution for a new iteration.