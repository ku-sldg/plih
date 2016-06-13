---
layout: blog
category: blog
title: Project 2 Info
---
A few more thoughts on Project 2...

First, it's been pointed out that the deferred substitution list
values are no longer just numbers.  Recall that the `value` field uses
the predicate `number?` in our earlier definitions.  Because our
interpreter now returns an element of WFAE, the type of the value
field should now be `WFAE?`.  This is a tiny change, but is critical
if you want to include functions in your deferred substitution list.

Second, I used the term *strict evaluation* in the project
description.  I hit this in class, but wanted to make sure you realize
that strict simply means arguments are evaluated before they are
passed into a function.  This is what we've done in class until now
and what Racket does.  Lazy is the opposite and is what Haskell does
by default.  Specifically, parameters are evaluated when used and not
before they are passed in.

