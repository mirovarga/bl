# bl

A single binary, easy to use static blog generator with a file server and
readonly REST API.

> It's my first real project in Haskell and there are still things I haven't
  implemented yet, like user-friendly (error) messages, better CLI and probably
  others.

- [Overview](#overview)
- [Installation](#installation)
- [Creating a Blog](#creating-a-blog)
- [Generating HTML Files](#generating-html-files)
- [Serving the Files](#serving-the-files)
- [REST API](#rest-api)

## Overview

`bl` (short for **bl**og) just needs posts written in
[Markdown](https://www.markdownguide.org/) and a set of
[Mustache](https://mustache.github.io/) templates to generate the posts to HTML
files.

It can then serve the files with a built-in file server or expose the posts via
a readonly REST API.

## Installation

Just [download a binary](https://github.com/mirovarga/bl/releases) and unzip it
to a directory of your choice.

> Ideally the directory should be on `PATH` so you can just type `bl` instead of
  `./bl`.

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

> For an example, see
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
standalone:   # yes | no, optional, default 'no'
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
  "standalone": "",
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

> Draft posts (with `draft: yes` in their front matter) are ignored when
  generating the files.

> Standalone posts (with `standalone: yes` in their front matter) aren't
  automatically added to the index and tag pages but have to be linked to
  manually in templates. 

### The `build` Command Reference

```
$ bl build -h
Usage: bl build [-d|--dir STRING]

Available options:
  -h,--help                Show this help text
  -d,--dir STRING          Path to the directory with posts and templates
                           (default: .)
```

## Serving the Files

`bl` includes a built-in file server. To start the server, `cd` to the blog's
directory and use the `fileserver` command:

```
$ bl fileserver
```

Now open [http://localhost:2703](http://localhost:2703) in your browser to see
the generated blog.

> You don't need to run the `build` command before starting the file server as
> it can re/generate HTML files before serving them. Just use the `-r` switch:
>  
>  ```
>  $ bl fileserver -r
>  ```

### The `fileserver` Command Reference

```
$ bl fileserver -h
Usage: bl fileserver [-d|--dir STRING] [-p|--port INT] [-r|--rebuild]

Available options:
  -h,--help                Show this help text
  -d,--dir STRING          Path to the directory with posts and templates
                           (default: .)
  -p,--port INT            Port to listen on (default: 2703)
  -r,--rebuild             Rebuild before serving (default: False)
```

## REST API

In addition to generating HTML files and serving them, `bl` also provides a 
readonly REST API to access the posts.

To start the API server, `cd` to the blog's directory and use the `apiserver`
command:

```
$ bl apiserver
```

Now `curl http://localhost:2703/posts/0` to fetch the newest post.

> You don't need to run the `build` command before starting the API server as it
  uses directly Markdown files from the `posts` directory.

### Endpoints

#### `/posts`

`GET /posts` returns all posts ordered from newest to oldest. You can filter
them with the following query parameters:

- `tag=<tag>` to return only posts with the `<tag>`
- `standalone=<yes|no>` to return only standalone or only non-standalone posts
   respectively
- `draft=<yes|no>` to return only draft or only non-draft posts respectively

#### `/posts/{index}`

`GET /posts/{index}` returns the post with the (zero-based) `index`.

#### `/posts/{key}`

`GET /posts/{key}` returns the post with the `key`.

> Multiple posts are sorted from newest to oldest.

> To see the API in action, you can use my blog's API server running at
  `https://mirovarga.com/api`.

### The `apiserver` Command Reference

```
$ bl apiserver -h
Usage: bl apiserver [-d|--dir STRING] [-p|--port INT]

Available options:
  -h,--help                Show this help text
  -d,--dir STRING          Path to the directory with posts and templates
                           (default: .)
  -p,--port INT            Port to listen on (default: 2703)
```
