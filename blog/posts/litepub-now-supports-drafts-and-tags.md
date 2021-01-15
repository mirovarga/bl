---
title: LitePub Now Supports Drafts and Tags
description: "In the previous couple of days I've been working on two new features
  for LitePub: drafts and tags."
created: 2015-11-17T00:00:00Z
tags: [LitePub, Go]
---

In the previous couple of days I've been working on two new features for
[LitePub](https://github.com/mirovarga/litepub): drafts and tags.

> LitePub is a lightweight static blog generator.

## Drafts

Any post can be marked as draft by simply moving it to the `draft` subdirectory
of the `posts` directory. To unmark it just move it back to the `posts`
directory.

## Tags

A post can include a comma separated tags, like this (line `5`):

```markdown
1 # How I Switched from Java to JavaScript
2
3 *Jan 25, 2015*
4
5 *Java, JavaScript*
6
7 I know that there are lots of posts about why JavaScript, or more specifically
8 Node.js, is better than Java but nevertheless I wanted to contribute, too.
```

> Download a [release](https://github.com/mirovarga/litepub/releases) to check
  it out. If you're new to LitePub follow
  the [quick start](https://github.com/mirovarga/litepub#quick-start) to get started.
