---
title: Installing Acrobat Reader on Ubuntu 14.04
description: Yesterday I was generating some PDFs with attachments in Java and
    needed to check if the attachments were really there.
created: 2015-11-21T00:00:00Z
tags: [Linux, How-to]
---

Yesterday I was generating some PDFs with attachments in Java (using
[Apache PDFBox](https://pdfbox.apache.org) if you're interested) and needed to
check if the attachments were really there.

Unfortunately, it seems neither [MuPDF](http://mupdf.com) nor
[Evince](https://wiki.gnome.org/Apps/Evince) support attachments (I didn't try
other viewers) so I had to install Acrobat Reader to check them.

As there's no official Linux package on the
[Adobe website](https://get.adobe.com/reader/otherversions) it's not so
straightforward to install it on Ubuntu. But it's not complicated either.

## The Installation

1. Download the `AdbeRdr9.5.5-1_i386linux_enu.deb` package:

    ```
    $ wget ftp://ftp.adobe.com/pub/adobe/reader/unix/9.x/9.5.5/enu/AdbeRdr9.5.5-1_i386linux_enu.deb
    ```

2. Install it:

    ```
    $ sudo dpkg -i AdbeRdr9.5.5-1_i386linux_enu.deb
    ```

    > It will finish with missing dependency errors, like this:

    ```
    Selecting previously unselected package adobereader-enu.
    (Reading database ... 120320 files and directories currently installed.)
    Preparing to unpack AdbeRdr9.5.5-1_i386linux_enu.deb ...
    Unpacking adobereader-enu (9.5.5) ...
    dpkg: dependency problems prevent configuration of adobereader-enu:
     adobereader-enu depends on libgtk2.0-0 (>= 2.4); however:
      Package libgtk2.0-0 is not installed.

    dpkg: error processing package adobereader-enu (--install):
     dependency problems - leaving unconfigured
    Processing triggers for man-db (2.6.7.1-1ubuntu1) ...
    Errors were encountered while processing:
     adobereader-enu
    ```

3. Install the missing dependencies:

    ```
    $ sudo apt-get -f install --no-install-recommends
    ```
    
    > `--no-install-recommends` is optional. I use it to avoid installing
      unneeded packages.

4. Everything should be fine now and you can run Acrobat Reader:

    ```
    $ acroread
    ```

That's it. I hope it helped you.
