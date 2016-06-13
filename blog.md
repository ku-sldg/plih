---
layout: frontpage
title: Chapters
---

# EECS 662 Blog

{% for post in site.categories.chapter %}
<a href="{{ site.baseurl }}{{ post.url }}">{{ post.title }}</a>

{{ post.content }}

-----

{% endfor %}
