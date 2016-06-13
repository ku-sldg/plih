---
layout: blog
category: blog
title: Project 2 testing
---
The question arose in class whether Racket's `test` function will
handle constructed values from `define-type`.  It appears that it
will.  It's not difficult to compare two values - just see if their
parts are the same - I just wasn't sure that Racket determined that
automatically.  It appears that it does.
