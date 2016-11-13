---
layout: frontpage
title: Chapters
---

# Book Chapters

{% for post in site.categories.chapter %}
<a href="{{ site.baseurl }}{{ post.url }}">{{ post.title }}</a>

{{ post.content }}

-----

{% endfor %}
