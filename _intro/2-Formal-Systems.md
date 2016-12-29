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

One way to think about the world of languages is in terms of _formal systems_.  Attributed to David Hilbert and Gotlieb Frege, a formal system provides mechanisms for representing and reasoning a out systems.  The term _formal_ implies principled or formal in a mathematical sense.

## Syntax

The _syntax_ of a formal system is 

Syntax is frequently defined using a _grammar_ as defined in Chapter 1.

The _alphabet_ of a grammar is a set of atomic symbols representing syntax elements that cannot be decomposed further.  The _rules_ of a grammar define legal orderings of symbols.  The set of strings that are in the closure of the alphabet with respect to application of grammar rules is defined as the formal language described by the grammar.

As an example, consider the grammar of a subset of propositional logic:

$$
\begin{align*}
t ::=\; & \ID \mid \ttrue \mid \ffalse \\
& t \wedge t \mid t \vee t \\
& t \Rightarrow t \mid \neg t \\
v ::=\; & \ttrue \mid \ffalse \\
\end{align*}
$$

This format should be familiar the previous chapter.  The alphabet includes terminal symbols including $\ttrue$ and $\ffalse$, but also symbols such as $\wedge$ and $\vee$.  The $\ID$ term is a shorthand for all identifiers representign propsitions.  Grammar rules define $\Rightarrow$, $\vee$, and \$wedge$ as binary operations and $\neg$ as a unary operator.  The recursive nature of grammar rules over $t$ allows arbitrary nesting of terms.

## Inference System

_Inference systems_ are defined by _axioms_ and _inference rules_.  The classical notation for inference rules was defined in the previous chapter with axioms simply defined as inference rules with no antecedents.  Again looking at propositional logic we start with one axiom that $\ttrue$ is always true:

$$\frac{}{true}$$

Nothing need be known to assert $\ttrue$ in a proof.

Other inference rules define introduction and elmination rules for various operators.  Introduction rules introduce their associated operation in an expression.  The introduction rule for $X\wedge Y$ is:

$$\frac{X,Y}{X\wedge Y}$$

If $X$ and $Y$ are both known, then $X\wedge Y$ is immediately true.

Elimination rules are the inverse of introduction rules.  There are two for $X\wedge Y$:

$$\frac{X\wedge Y}{X}\;\;\;\;\frac{X\wedge Y}{Y}$$

Each rule allows one conjunct to be infered from the conjunction.  The first giving the left conjunct and the second the right.

The elimination rule for $\neg$ is the double negative rule that should be intuitive:

$$\frac{\neg\neg X}{X}$$

The introduction rule for $\neg$ is more interesting as a derivation is one of the antecents.  The notation $X\vdash Y$ says that $Y$ is derivable from $X$.  The antecedent of the elimination rule says that assuming $X$ gives $Y$ and $\neg Y$ is also known.  This is a contradiction because $X$ and $\not X$ cannot be simultaneously true.  Thus, $X$ must be false:

$$\frac{X\vdash Y, \neg Y}{\neg X}$$

This is the classic proof by contradiction.  It also suggests that if $\ffalse$ is ever true, we have big problems because we can derive anything.

The rules for implication again eliminate and introduction $X\Rightarrow Y$.  The elimination rule is known as _modus ponens_ and says that if $X$ and $X\Rightarrow Y$ are known, then $Y$ is also known:

$$\frac{X,X\Rightarrow Y}{Y}$$

The introduction rule again has a derivation in the antecedent.  If assuming $X$ allows us to derive $Y$, then we also know that $X\Rightarrow Y$:

$$\frac{X\vdash Y}{X\Rightarrow Y}$$

Now we have a subset of the inference rules for propositional logic.  How do we use them?  Let's do a quick proof:

$$\cfrac{\cfrac{A\wedge B}{B}\cfrac{A\wedge B}{A}}{B\wedge A}$$


## Semantics

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
	<tr><th>X</th> <th>$\neg$ X</th></tr>
	<tr><td>T</td> <td>F</td></tr>
	<tr><td>F</td> <td>T</td></tr>
</table>



