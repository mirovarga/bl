# bl

A single binary, easy to use static blog generator with a file server and
readonly REST API.

> It's a rewrite of [LitePub](https://github.com/mirovarga/litepub) in Haskell.

## Table of Contents

- [Overview](#overview)
- [Creating a Blog](#creating-a-blog)
  - [Directory Structure](#directory-structure)
  - [Posts](#posts)
  - [Templates](#templates)
    - [Data Passed to Templates](#data-passed-to-templates)
- [Generating HTML Files](#generating-html-files)
- [Serving the Files](#serving-the-files)
- [REST API](#rest-api)
  - [Endpoints](#endpoints)

## Overview

`bl` (short for *bl*og) just needs posts written in
[Markdown](https://www.markdownguide.org/) and a set of
[Mustache](https://mustache.github.io/) templates to generate the posts to HTML
files.

It can then serve the files with a built-in file server or expose the posts via
a readonly REST API.

## Creating a Blog

### Directory Structure

Each blog is stored in a directory with the following structure:

```bash
posts/              # the posts
templates/          # the templates and accompanying files (styles, scripts, images, etc.)
  index.mustache
  post.mustache
  tag.mustache
static/             # the generated HTML files (plus copied accompanying files)
```

> For examples, see
  [my blog](https://github.com/mirovarga/mirovarga.com).

### Posts

Posts are written in [Markdown](https://www.markdownguide.org/) with a YAML
front matter at the start of each post.

```yaml
---
title:
description:  # optional
created:      # YYYY-MM-DDTHH:MM:SSZ, e.g. 2021-02-10T00:00:00Z
tags:         # [], e.g. [bl, Haskell], optional
draft:        # yes | no, optional, default 'no'
key:          # optional, default slugified title
---
```

> For examples, see
  [posts of my blog](https://github.com/mirovarga/mirovarga.com/tree/master/posts).

### Templates

Templates are written in [Mustache](https://mustache.github.io/). There are
three required files in the `templates` directory:

- `index.mustache` for generating the index page
- `post.mustache` for generating a single post
- `tag.mustache` for generating a tag page

> For examples, see
  [templates of my blog](https://github.com/mirovarga/mirovarga.com/tree/master/templates).

#### Data Passed to Templates

The primary data is the post object:

```javascript
{
  "title": "",
  "description": "",
  "created": "",
  "tags": [
    {
      "camelCase": "",    // user-friendly tag name
      "lowerCase": ""     // tag name used in the file name of the generated page 
    },
  ],
  "draft": "",
  "key": "",
  "content": ""           // the post's Markdown rendered to HTML   
}
```

The `index.mustache` template is passed a list of post objects:

```javascript
[
    { /* ... */ }
]
```

The `post.mustache` template is passed a single post object:

```javascript
[ /* ... */ ]
```

The `tag.mustache` template is passed an object with the tag name and a list of
post objects:

```javascript
{
  "tag": "",
  "posts": [
    { /* ... */ }
  ]
}
```

## Generating HTML Files

To generate the HTML files for a blog, `cd` to the blog's directory and use the
`build` command:

```
$ bl build
```

The files are generated to the `static` directory. Also, all accompanying files
(and directories) from the `templates` directory are copied to the `static`
directory.

## Serving the Files

`bl` includes a built-in file server. To start the server, `cd` to the blog's
directory and use the `fileserver` command:

```
$ bl fileserver
```

Now open [http://localhost:2703](http://localhost:2703) in your browser to see
the generated blog.

## REST API

In addition to generating HTML files and serving them, `bl` also provides a 
readonly REST API to access the posts.

To start the API server, `cd` to the blog's directory and use the `apiserver`
command:

```
$ bl apiserver
```

Now `curl http://localhost:2703/posts/0` to fetch the newest post.

### Endpoints

- `/posts` fetches all posts
- `/posts?tag={tag}` fetches posts tagged with `tag`
- `/posts/{index}` fetches the post with the (zero-based) `index`
- `/posts/{key}` fetches the post with the `key`

> Multiple posts are sorted from newest to oldest.
