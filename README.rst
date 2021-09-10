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

Right now there is no a binary to install, you need a `LispWorks`_ to load and run program.
Here is how to install it and run built-in plugins.

Fist, add an Ultralisp quicklisp distribution containing `LispWorks`_ extensions:

.. code::

   (ql-dist:install-dist "http://dist.ultralisp.org/lispworks.txt"
                         :prompt nil)

Next, load ``Barista`` with built-in plugins and run them:

.. code::

   (ql:quickload :barista)
   (ql:quickload :barista-plugins)
   (barista/main:start-plugins)

Roadmap
=======

* Implement few useful plugins.
* Add a documentation.
* Create a distribution installable by Homebrew.
* Publish app to App Store.

.. _XBar: https://github.com/matryer/xbar
.. _TextBar: http://richsomerfield.com/apps/textbar/
.. _LispWorks: http://www.lispworks.com/
