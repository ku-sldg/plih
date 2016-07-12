---
layout: frontpage
title: Programming Languages in Haskell
---

# Programming Languages in Haskell

Support for EECS 662 at The University of Kansas.

This is not even remotely close to done or ready for use.  Plenty of wrong things or things not defined well.  It's coming though.  Version 0.0 will be ready January 2017 when EECS 662 is taught next.  For now, you can enjoy watching the struggles of going from a great idea to an actual book.

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


