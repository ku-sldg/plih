---
layout: frontpage
title: Formal Systems
use_math: true
categories: chapter ch1
---

$$
\newcommand\calc{\mathsf{calc}\;}
\newcommand\parse{\mathsf{parse}\;}
\newcommand\typeof{\mathsf{typeof}\;}
\newcommand\interp{\mathsf{interp}\;}
\newcommand\eval{\mathsf{calc}\;}
\newcommand\NUM{\mathsf{NUM}}
\newcommand\NAT{\mathsf{NAT}}
\newcommand\ID{\mathsf{ID}}
\newcommand\ttrue{\mathsf{true}}
\newcommand\ffalse{\mathsf{false}}
$$

One way to think about the world of languages (or the world in general) is in terms of _formal systems_.  Attributed to David Hilbert and Gotlieb Frege, a formal system provides mechanisms for representing and reasoning a out systems.  The term _formal_ implies principled or formal in a mathematical sense.

## Syntax

The _syntax_ of a formal system defines the form of _terms_ in that system.  Syntax is frequently defined using a _grammar_ as defined in Chapter 1.  We're not going to do much with syntax, so little needs to be said other than providing a basic definition.

The _alphabet_ of a grammar is a set of atomic symbols representing syntax elements that cannot be decomposed further.  The _rules_ of a grammar define legal orderings of symbols.  The set of strings that are in the closure of the alphabet with respect to application of grammar rules is defined as the formal language described by the grammar.

As an example, consider the grammar of a subset of propositional logic:

$$
\begin{align*}
t ::=\; & \ID \mid \ttrue \mid \ffalse \\
& t \wedge t \mid t \vee t \mid \neg t\\
& t \Rightarrow t \mid t \Leftrightarrow t \\
v ::=\; & \ttrue \mid \ffalse \\
\end{align*}
$$

This format should be familiar from the previous chapter and from earlier studies.  The alphabet includes terminal symbols including $\ttrue$ and $\ffalse$, but also symbols such as $\wedge$ and $\vee$.  The $\ID$ term is a shorthand for all identifiers representing propositions.  Grammar rules define $\Rightarrow$, $\Leftrightarrow$, $\vee$, and $\wedge$ as binary operations and $\neg$ as a unary operator.  The recursive nature of grammar rules over $t$ allows arbitrary nesting of terms.

## Inference System

_Inference systems_ are defined by _axioms_ and _inference rules_. They allow derivation of true statements form other true statements.  You do this whenever you use mathematics to simplify a formula or solve equations for a quantity.  We all know, for example, that $(x+y)^2 = x^2+2xy+y^2$.  We also know that $(x+y)^2 = x^2+2xy+y^2$ is true in algebra regardless of what $x$ and $y$ represent.  We could define this relationship using inference rules:

$$\frac{(x+y)^2}{x^2+2xy+y^2}\;\;\;\;\frac{x^2+2xy+y^2}{(x+y)^2}$$

or an equivalence:

$$(x+y)^2 == x^2+2xy+y^2$$

The classical notation for inference rules was defined in the previous chapter.  The inference rule:

$$\frac{A_0,A_1,\ldots,A_n}{C}$$

states that when $A_0$ through $A_n$ are true, then $C$ is also true.  The $A_k$'s are often referred to as _antecedents_ while $C$ is the _consequent_.  The set of antecedents may be arbitrarily large, but there is only one consequent associated with a rule.  The special case when the set of antecedents is empty:

$$\frac{}{A}$$

defines an axiom.  Nothing need be true for $A$ to be true, therefore it is always true.

As an example inference system we'll look at _propositional logic_, the logic of true and false propositions that defines the heart of classical logic.  We'll start with one axiom that $\ttrue$ is always true:

$$\frac{}{true}$$

Nothing need be known to assert $\ttrue$ is trivially true.  It turns out that $\ttrue$ doesn't tell us much, but it does serve as a value in our logic.  The other value $\ffalse$.  Consider what axioms or inference rules might have $\ffalse$ as a consequent.  Are there any?

Other inference rules define introduction and elimination rules for various operators.  Introduction rules introduce their associated operation in an expression.  The introduction rule for $X\wedge Y$ is:

$$\frac{X,Y}{X\wedge Y}$$

If $X$ and $Y$ are both known, then $X\wedge Y$ is immediately true.

Elimination rules are the inverse of introduction rules.  There are two for $X\wedge Y$:

$$\frac{X\wedge Y}{X}\;\;\;\;\frac{X\wedge Y}{Y}$$

Each rule allows one conjunct to be inferred from the conjunction.  The first giving the left conjunct and the second the right.  Note that introduction rules make larger terms from smaller term while elimination rules make smaller terms from larger terms.  This will have important consequences when we talk about proofs.

Speaking of proofs, we now have a tiny subset of the inference rules defining propositional logic.  How do we use them?  Let's do a quick derivation that
combines inference rules:

$$\cfrac{\cfrac{A\wedge B}{B}\;\;\;\; \cfrac{A\wedge B}{A}}{B\wedge A}$$

Now we can say $A\wedge B \vdash B\wedge A$.  We can also prove the inverse:

$$\cfrac{\cfrac{B\wedge A}{A}\;\;\;\; \cfrac{B\wedge A}{B}}{A\wedge B}$$

Now we can say $B\wedge A \vdash A\wedge B$.

The elimination rule for $\neg$ is the double negative rule from classical logic:

$$\frac{\neg\neg X}{X}$$

This rule is frequently call the double negation rule and simply says that the negation of the negation of any term is the term.

The introduction rule for $\neg$ is more interesting as a derivation is one of the antecedents.  The notation $X\vdash Y$ says that $Y$ is _derivable_ from $X$.

The antecedent of the elimination rule says that assuming $X$ gives $Y$ and $\neg Y$ is also known.  This is a contradiction because $X$ and $\neg X$ cannot be simultaneously true.  Thus, $X$ must be false:

$$\frac{X\vdash Y, \neg Y}{\neg X}$$

This is the classic proof by contradiction.  It also suggests that if $\ffalse$ is ever true, we have big problems because we can derive anything.

The rules for implication again eliminate and introduction $X\Rightarrow Y$.  The elimination rule is known as _modus ponens_ and says that if $X$ and $X\Rightarrow Y$ are known, then $Y$ is also known:

$$\frac{X,X\Rightarrow Y}{Y}$$

The introduction rule again has a derivation in the antecedent.  If assuming $X$ allows us to derive $Y$, then we also know that $X\Rightarrow Y$:

$$\frac{X\vdash Y}{X\Rightarrow Y}$$

Finally, we have introduction and elimination rules for logical equivalence.

$$\frac{X\Rightarrow Y\;\;\; Y\Rightarrow X}{X\Leftrightarrow Y}$$

$$\frac{X\Leftrightarrow Y}{X\Rightarrow Y}\;\;\;\;\frac{X\Leftrightarrow Y}{Y\Rightarrow X}$$

Using the implication introduction rule we can go a step farther and prove logical equivalence:

$$\cfrac{\cfrac{A\wedge B \vdash B\wedge A}{A\wedge B \Rightarrow B\wedge A}\;\;\;\; \cfrac{B\wedge A \vdash A\wedge B}{B\wedge A \Rightarrow A\wedge B}}{A\wedge B \Leftrightarrow B\wedge A}$$

There is much more we can do with inference rules and systems, but this brief demonstration should give you an idea of how these things define formal reasoning processes.  Just like Lego, simple things fit together in simple ways to develop complex and elegant systems.

## Semantics

A language's semantics gives its structures meaning.  When we used inference rules to define how we reason about propositional logic, we provided a reasoning mechanism without regard to meaning.  We could have changed the inference rules in a very simple way and gotten something that is not at all propositional logic.  Let's say we defined a completely wrong rule for implication like this:

$$\frac{Y,X\Rightarrow Y}{X}$$

Clearly this is not how implication works, but it is a perfectly fine rule and we can reach conclusions from it.  What makes it incorrect is the semantics of propositional logic.  Semantics defines the meanings of language expressions using another mathematical system.

For propositional logic we can use the common notion of a truth table to define our operations:

<style>
  table {
    border-collapse: collapse;
    width: 20%;
  }
  th, td {
    border: 0px solid #ccc;
    padding: 1px;
	height: 5px;
	text-align: center;
	vertical-align: center;
  }
</style>
<table>
	<tr><th>X</th> <th>Y</th> <th>X $\wedge$ Y</th></tr>
	<tr><td>F</td> <td>F</td> <td>F</td></tr>
	<tr><td>F</td> <td>T</td> <td>F</td></tr>
	<tr><td>T</td> <td>F</td> <td>F</td></tr>
	<tr><td>T</td> <td>T</td> <td>T</td></tr>
</table>

<table>
	<tr><th>X</th> <th>Y</th> <th>X $\vee$ Y</th></tr>
	<tr><td>F</td> <td>F</td> <td>F</td></tr>
	<tr><td>F</td> <td>T</td> <td>T</td></tr>
	<tr><td>T</td> <td>F</td> <td>T</td></tr>
	<tr><td>T</td> <td>T</td> <td>T</td></tr>
</table>

<table>
	<tr><th>X</th> <th>Y</th> <th>X $\Rightarrow$ Y</th></tr>
	<tr><td>F</td> <td>F</td> <td>T</td></tr>
	<tr><td>F</td> <td>T</td> <td>T</td></tr>
	<tr><td>T</td> <td>F</td> <td>F</td></tr>
	<tr><td>T</td> <td>T</td> <td>T</td></tr>
</table>

<table>
	<tr><th>X</th> <th>Y</th> <th>X $\Leftrightarrow$ Y</th></tr>
	<tr><td>F</td> <td>F</td> <td>T</td></tr>
	<tr><td>F</td> <td>T</td> <td>F</td></tr>
	<tr><td>T</td> <td>F</td> <td>F</td></tr>
	<tr><td>T</td> <td>T</td> <td>T</td></tr>
</table>

<table>
	<tr><th>X</th> <th>$\neg$ X</th></tr>
	<tr><td>T</td> <td>F</td></tr>
	<tr><td>F</td> <td>T</td></tr>
</table>

We can't easily derive new truths using simple truth tables, but we can with the inference system.  To ensure the inference system only produces correct results we can compare it with what is specified in the truth tables.  Let's look at our broken rule for negation:

$$\frac{Y,X\Rightarrow Y}{X}$$

The rule says that if $Y$ and $X\Rightarrow Y$ are both true, then  $X$ must also be true.  Looking at the truth table for $\Rightarrow$ clearly says otherwise.  When $Y$ is true and $X\Rightarrow Y$ is true in the second row, $X$ is false.

## Discussion

Thinking of languages and mathematical systems as formal systems will serve you well.  Throughout this writing we will think of languages in terms of what they look like (syntax), how to manipulate them (inference system), and what they mean (semantics).  At times the Hilbert system structure will be difficult to see, but it is always there.

## Exercises
