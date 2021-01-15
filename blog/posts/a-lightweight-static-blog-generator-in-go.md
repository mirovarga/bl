---
title: A Lightweight Static Blog Generator in Go
description: From its start, this blog had been a custom built website using a
   combination of Jade for templates and Markdown for posts that were then
   generated to static HTML files by Harp.
created: 2015-11-12T00:00:00Z
tags: [LitePub, Go]
---

From its start, this blog had been a custom built website using a combination of
[Jade](http://jade-lang.com) for templates and Markdown for posts that were then
generated to static HTML files by [Harp](http://harpjs.com).

## Why Another Static Blog Generator?

I wanted to switch to some static blog generator because it was a little tedious
to add new posts to the blog, but couldn't decide which one to use.

What I looked for:

1. *No installation*. Just download a binary and that's all.

2. *Easy to use*. Simply create a Markdown file in the posts directory and
   that's enough to add a new post. No need for databases, projects,
   configurations, etc.

3. *No metadata*. No need to include special metadata in posts, like title, date
   or tags. It should be possible to parse these things from the post structure.
   In other words, I wanted a post to look like a post on its own.

## Introducing LitePub

As I couldn't find a static blog generator that would meet my needs I decided to
write [LitePub](https://github.com/mirovarga/litepub). I use it to generate this
blog and it meets all my requirements:

- it's a single binary
- to create a new post I just add an `.md` file to the `posts` directory
- title, date and tags (not supported yet) are parsed from the first lines of
  posts

> If you're interested,
  download a [release](https://github.com/mirovarga/litepub/releases) and follow
  the [quick start](https://github.com/mirovarga/litepub#quick-start) to create
  a sample blog.
