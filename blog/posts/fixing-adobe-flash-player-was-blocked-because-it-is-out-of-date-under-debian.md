---
title: Fixing 'Adobe Flash Player was blocked because it is out of date' under Debian
description: After updating Chromium to version 59 under Debian 64 I was getting
    a message saying that 'Adobe Flash Player was blocked because it is out of
    date' whenever I opened a site with a Flash content.
created: 2017-06-13T00:00:00Z
tags: [Linux, How-to]
---

After updating Chromium to version 59 under Debian 64 I was getting a message
saying that 'Adobe Flash Player was blocked because it is out of date' whenever
I opened a site with a Flash content.

## The Fix

1. Uninstall the out-of-date version:

    ```
    $ sudo apt-get remove pepperflashplugin-nonfree
    ```

2. Download the up-to-date version:

    ```
    $ wget https://fpdownload.adobe.com/pub/flashplayer/pdc/26.0.0.126/flash_player_ppapi_linux.x86_64.tar.gz
    ```

3. Unpack it:

    ```
    $ tar xzf flash_player_ppapi_linux.x86_64.tar.gz
    ```

4. Copy the `libpepflashplayer.so` file to the `/usr/lib/chromium/plugins`
   directory (create it if it doesn't exist already):   

    ```
    $ sudo mkdir /usr/lib/chromium/plugins
    $ sudo cp libpepflashplayer.so /usr/lib/chromium/plugins
    ```

5. Tell Chromium where to look for Flash Player:

    ```
    $ sudo echo 'export CHROMIUM_FLAGS="$CHROMIUM_FLAGS --ppapi-flash-path=/usr/lib/chromium/plugins/ \ 
    libpepflashplayer.so --ppapi-flash-version=26.0.0.126 --disable-bundled-ppapi-flash"' >> /etc/ \
    chromium.d/default-flags
    ```
    
    > The `--disable-bundled-ppapi-flash` flag is probably not needed but one
      never knows :)
    
6. Close all Chromium windows and run Chromium again.
    
    > **NB**: Typing `chrome://restart` in a tab won't update the Chromium flags
      so Chromium will still use the out-of-date version of Flash Player.
   
That's it. I hope it helped you.
