# High Performance Stencil Code Generation with Lift
Paper by Bastian Hagedorn et al.  
Presentation by Nathan Oerlemans  
Summary by Simon van Hus (6147879)

---

This paper focusses on extending the Lift programming language and compiler with primitives and a rewrite rule for stencils.
Stencils are used in many domain specific languages, however while they are easily parallelized there is no generally usable implementation.
The authors hope by extending the Lift compiler for stencil operations, they can change that.
(The paper is from 2017, and the Lift compiler and programming languages have both since been abandoned.)

The three main operations needed for stencils are the stencil function itself, the boundary function, and some source array.
The authors wish to use Lift's built-in map primitive to allow for stencils in Lift.
To this end they need two added primitives: slide and pad.
Pad can be the boundary function, by pulling an item from the source array when needed.
Slide can help select on which items in the source array to actually apply the stencil function (to establish a neighbourhood).
Finally they also add a rewrite rules, to deal with overlapping tiles in a the execution of a stencil (as they only need to be accessed once).

The authors show how the new primitives can be used to emulate even very complex stencils.
And they compare the performance of the Lift extension to that of a similar high-end GPU code compiler, which Lift outperforms.

<br />
<img
    src="./memes/more speed please.jpg"
    style="width: 55%; margin-left: 22.5%;"
    title="Bernie Sander with subtitles 'I'm once again asking for more speed'"
    alt="Bernie Sander with subtitles 'I'm once again asking for more speed'" />