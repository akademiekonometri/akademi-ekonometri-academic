---
# Documentation: https://sourcethemes.com/academic/docs/managing-content/

draft: false
title: "{{ replace .Name "-" " " | title }}"
author: Akademi Ekonometri
date: {{ .Date }}
slug: 
categories: []
tags: []
subtitle: ""
summary: ""
lastmod: {{ .Date }}
publishDate: ""
featured: false
disable_jquery: false

# Featured image
# To use, add an image named `featured.jpg/png` to your page's folder.
# Focal points: Smart, Center, TopLeft, Top, TopRight, Left, Right, BottomLeft, Bottom, BottomRight.
image:
  caption: "[Photo from Unsplash](https://unsplash.com/)"
  focal_point: ""
  preview_only: false

# Projects (optional).
#   Associate this post with one or more of your projects.
#   Simply enter your project's folder or file name without extension.
#   E.g. `projects = ["internal-project"]` references `content/project/deep-learning/index.md`.
#   Otherwise, set `projects = []`.
projects: []
disable_codefolding: false ## Set to true to disable code folding.
codefolding_show: "show" ## Set to "hide" or "show" all codes by default.
codefolding_nobutton: false ## Set to true to exclude the "Show/hide all" button.
output:
  blogdown::html_page:
    number_sections: true
    toc: true
    toc_depth: 2
    mathjax: local
    self_contained: false
# links:
# - icon: flask
#   icon_pack: fas
#   name: demo slides
#   url: ../../slides/rladies-demo-slides.html
# - icon: github
#   icon_pack: fab
#   name: demo code
#   url: https://example.com
---
