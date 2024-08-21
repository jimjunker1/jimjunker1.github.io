---
title: Welcome
date: "2022-05-02"
layout: collection
knit: (function(inputFile, encoding){
       rmarkdown::render(inputFile,
       encoding=encoding,
       output_file=here::here("_musings/Initial-post.md"))})
categories:
  - Miscellaneous
tags:
  - content
output:
  md_document:
    preserve_yaml: TRUE
    variant: markdown_strict 
---

This will be a space dedicated to things I find interesting and/or
useful. There will be no strict rules but I hope to contribute writings
centered on varying topics: working through specific coding problems,
personal learning progress, interesting thoughts or visualizations, or
whatever.
