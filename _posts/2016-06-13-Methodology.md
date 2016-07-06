---
layout: frontpage
title: Adding Identifiers
use_math: true
category: chapter
---

$$
\newcommand\calc{\mathsf{calc}\;}
\newcommand\parse{\mathsf{parse}\;}
\newcommand\typeof{\mathsf{typeof}\;}
\newcommand\interp{\mathsf{interp}\;}
\newcommand\eval{\mathsf{eval}\;}
\newcommand\NUM{\mathsf{NUM}\;}
\newcommand\ID{\mathsf{ID}\;}
\newcommand\iif{\mathsf{if}\;}
\newcommand\tthen{\;\mathsf{then}\;}
\newcommand\eelse{\;\mathsf{else}\;}
\newcommand\iisZero{\mathsf{isZero}\;}
\newcommand\bbind{\mathsf{bind}\;}
\newcommand\iin{\mathsf{in}\;}
\newcommand\aand{\;\mathsf{\&\&}\;}
\newcommand\lleq{\;\mathtt{<=}\;}
\newcommand\ttrue{\;\mathsf{true}}
\newcommand\ffalse{\;\mathsf{false}}
\newcommand\tnum{\;\mathsf{TNum}}
\newcommand\tbool{\;\mathsf{TBool}}
$$

# Methodology

Before charging forward into our next interpreter, let's pause for a moment and discuss a general methodology that is reflected in `AE` and `ABE`.

1. Concrete syntax
2. Abstract syntax
3. Pretty Printer
4. Parser
5. Evaluation rules
6. Type Rules
7. Evaluation relation
8. Typeof
9. Generator

Abbreviated

1. Abstract Syntax
2. Evaluation rules
3. Type rules
4. Evaluation relation
5. Typeof relation

## Concrete Syntax

Define a formal grammar by adding new constructs

## Abstract Syntax

Define constructors for new concrete syntax

## Parser

Extend parser to transform new concrete syntax into new abstract syntax

## Pretty Printer

Extend pretty printer to transform new abstract syntax into new concrete syntax

## Evaluation Rules

Define formal rules for evaluating new constructs

## Type Rules

Define formal rules for type checking new constructs

## Evaluation

Update `eval` to include new constructs

## Type Inference

Update `typeof` to include new constructs

## Generator

Update generator to include new constructs