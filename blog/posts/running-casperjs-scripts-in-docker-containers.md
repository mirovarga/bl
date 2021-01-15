---
title: Running CasperJS Scripts in Docker Containers
description: I used to run the casperbox.com service (now shut down) that
  enabled to run CasperJS scripts online.
created: 2015-06-16T00:00:00Z
tags: [Docker, JavaScript]
---

I used to run the *casperbox.com* service (now shut down) that enabled to run
[CasperJS](http://casperjs.org) scripts online. For security reasons, each script
was run in a separate Docker container.

Recently I was asked by a former user of the service if I could release the code
that managed starting the containers and running scripts.

So over the weekend I put together a slightly modified version of the original
Node.js module.

> If you're interested you can check it out at
  [GitHub](https://github.com/mirovarga/casperjs-docker).
