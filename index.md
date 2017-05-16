---
layout: frontpage
title: Programming Languages in Haskell
---

# Programming Languages in Haskell

An introduction to programming languages using Haskell.  Written for EECS 662 at The University of Kansas.

---

# Acknowledgements

{% for post in site.front %}
## <a href="{{ site.baseurl }}{{ post.url }}">{{ post.title }}</a>
{% endfor %}

---

# Introduction

{% for post in site.intro %}
## <a href="{{ site.baseurl }}{{ post.url }}">{{ post.title }}</a>
{% endfor %}

---

# Simple Interpreters

{% for post in site.simple %}
## <a href="{{ site.baseurl }}{{ post.url }}">{{ post.title }}</a>
{% endfor %}

---

# Identifiers

{% for post in site.ids %}
## <a href="{{ site.baseurl }}{{ post.url }}">{{ post.title }}</a>
{% endfor %}

---

# Functions

{% for post in site.funs %}
## <a href="{{ site.baseurl }}{{ post.url }}">{{ post.title }}</a>
{% endfor %}

---

# Typed Functions

{% for post in site.types %}
## <a href="{{ site.baseurl }}{{ post.url }}">{{ post.title }}</a>
{% endfor %}

---

# State

{% for post in site.state %}
## <a href="{{ site.baseurl }}{{ post.url }}">{{ post.title }}</a>
{% endfor %}
