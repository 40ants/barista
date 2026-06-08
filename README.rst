=========
 Barista
=========

.. image:: images/pomodoro.gif
   :alt: Pomodoro Plugin Demo

This programm allows you to put any information you need into the OSX's menu bar.

There is similar software - the `XBar`_ and `TextBar`_. The difference between the XBar and Barista:

* XBar periodically starts each plugin as a subprocess, Barista runs them as threads in the main process.
* With XBar, you need a place to keep state between the process invocations.
* With Barista, you just have all state in memory and don't need to bother.
* You can write plugins for XBar in any language, but most plugins are written in BASH.
* XBar's API forces you to use ``text`` to describe menu items.
* You are using powerful Common Lisp language, to implement plugins for Barista.
* Because of Lisp, you can debug your plugins, connecting the IDE to Barista process.

TextBar is similar to the XBar, but seems more featureful because it allows to render a dropdown
as HTML and to display simple graphs in the menu bar. Probably, some of these features will be added
to the Barista too.

Installation
============

Download the latest ``Barista-x.y.z.dmg`` from the `GitHub Releases`_ page,
open it and drag ``Barista.app`` to your ``Applications`` folder.

Because the app is not signed with an Apple Developer certificate, macOS will
attach a quarantine flag to it when downloaded from the internet and refuse to
open it with a *"Barista is damaged and can't be opened"* message.

To remove the quarantine flag, run the following command in Terminal after
copying the app to ``/Applications``:

.. code:: bash

   xattr -dr com.apple.quarantine /Applications/Barista.app

Then open the app normally.

.. _GitHub Releases: https://github.com/40ants/barista/releases

Roadmap
=======

* Implement few useful plugins.
* Add a documentation.
* Create a distribution installable by Homebrew.
* Publish app to App Store.

.. _XBar: https://github.com/matryer/xbar
.. _TextBar: http://richsomerfield.com/apps/textbar/
.. _LispWorks: http://www.lispworks.com/
