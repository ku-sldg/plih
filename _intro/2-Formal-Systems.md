---
layout: frontpage
title: Formal Systems
use_math: true
categories: chapter ch1
---
One way to think about the world is in terms of _formal systems_.  Attributed to David Hilbert and Gotlieb Frege, a formal system provides mechanisms for representing and reasoning a out systems.  The term _formal_ implies principled or formal in a mathematical sense.

## Syntax

The _syntax_ of a formal system is 

Syntax is frequently defined using a _grammar_ as defined in Chapter 1.

The _alphabet_ of a grammar is a set of atomic symbols representing syntax elements that cannot be decomposed further.  The _rules_ of a grammar define legal orderings of symbols.  The set of strings that are in the closure of the alphabet with respect to application of grammar rules is defined as the formal language described by the grammar.

$$\begin{align*}
t ::=\; & true \mid false \\
& t \wedge t \mid t \vee t \\
& t \Rightarrow t \mid \neg t \\
\end{align*}$$

## Inference System

## Semantics
