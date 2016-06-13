---
layout: blog
category: blog
title: Project 1 Odds and Ends - Updated
---
Some general thoughts and hints on Project 1.  Updated Feb 23

### Minor Changes

I've changed the example code in the project description to use the
name `binop-rec` rather than `binop` to hold binary operations in the
`binop` table.  There is no change in function, but it eliminates
confusion between `binop` in the table and `binop` in the abstract syntax.

### Binop Table and Lookup

The `binop` table is simply a list of `binop` structures.  If you
follow the data types I specified in the project, an example `binop`
table would look like this:

{% highlight scheme %} 
(list
  (binop-ref 'add +)
  (binop-ref 'sub -))
{% endhighlight %} 

You can pass this in as a parameter whenever you call `lookup`.  For
testing, I always assign these to global variables and pass in the
variable to avoid typing:

{% highlight scheme %} 
(define *binop-table*
  (list
    (binop-ref 'add +)
    (binop-ref 'sub -)))
{% endhighlight %} 

You can then call lookup like this:

{% highlight scheme %} 
(lookup op-name *binop-table*)
{% endhighlight %} 

### Abstract Syntax

In the project text there is an example of creating a binary operation
durign parsing that looks like this:

{% highlight scheme %}
(binop (op 'plus) (num 1) (num 2))
{% endhighlight %}

If you don't want to create a data type for operators, you don't need
to.  Specifically, you can define the first argument to binop as
`string?` implying that your structure creation will look like this:

{% highlight scheme %}
(binop 'plus (num 1) (num 2))
{% endhighlight %}

Note that `op` is gone.  You may find this less confusing.

### Accessors

Accessors are functions that pull elements out of data type
constructions.  Specifically, if you have the type declaration:

{% highlight scheme %}
(define-type blah
  (a-blah (a integer?) (b symbol?))
  (b-blah (a symbol?) (b? integer?)))
{% endhighlight %}

you can access elements of an instance of the type using the field
name appended to the constructor name.  If you want to grap the `a`
element from an `a-blah` you would do this:

{% highlight scheme %}
(define-type blah
  (a-blah (a integer?) (b symbol?))
  (b-blah (a symbol?) (b integer?)))
{% endhighlight %}

you can access elements of an instance of the type using the field
name appended to the constructor name.  If you want to grap the `a`
element from an `a-blah` you would do this:

{% highlight scheme %}
(a-blah-a (a-blah 1 'a))
{% endhighlight %}

which evaluates to `1`.  Similarly:

{% highlight scheme %}
(b-blah-b (b-blah 'b 2))
{% endhighlight %}

evalutes to `2`.
