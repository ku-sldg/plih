---
layout: frontpage
title: Programming Languages in Haskell
---

# Programming Languages in Haskell

Support for EECS 662 at The University of Kansas.

This is not even remotely close to done or ready for use.  Plenty of wrong things or things not defined well.  It's coming though.  Version 0.0 will be ready January 2017 when EECS 662 is taught next.  For now, you can enjoy watching the struggles of going from a great idea to an actual book.

---

{% for post in site.categories.ch0 %}
## <a href="{{ site.baseurl }}{{ post.url }}">{{ post.title }}</a>
{% endfor %}

---

{% for post in site.categories.ch1 %}
## <a href="{{ site.baseurl }}{{ post.url }}">{{ post.title }}</a>
{% endfor %}

---

{% for post in site.categories.ch2 %}
## <a href="{{ site.baseurl }}{{ post.url }}">{{ post.title }}</a>
{% endfor %}

---


