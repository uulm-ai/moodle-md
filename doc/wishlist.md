#Goal
Define a markdown dialect that can be converted to [Moodle-XML](https://docs.moodle.org/23/en/Moodle_XML_format).

#Restriction

- support only HTML5 browsers

##Basic Features

- question name (mandatory?)
- support formulas in TeX syntax that get converted to MathML
- support quiz options
- support answer feedback

#Question Types

- multichoice: fraction per answer, single, shuffleanswers, answernumbering
- shortanswer: fraction per answer
- truefalse: feedback for true/false
- numerical: tolerance, fraction/feedback per answer


#Example

#Question name {

Question body is found here.
```{tikz}
Can include interpreted code for tikz or graphviz
```

Can also use formulas $\sum_i \frac{x^i}{i}$!

Can also include item lists

 - like this
 - or this

##Answer

- {.correct} each answer is a single bullet point
- {.wrong} this is a wrong answer
- {.fraction = 50} this counts only for half of the points

##Feedback
General question feedback can be included here.

 - 
