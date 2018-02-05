[![License: GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://secure.travis-ci.org/doublep/extmap.png)](http://travis-ci.org/doublep/extmap)


# extmap

`extmap` is a very simple package that lets you build a *read-only*,
*constant* database that maps Elisp symbols to almost arbitrary Elisp
objects.  The idea is to avoid preloading all data to memory and only
retrieve it when needed.

This package doesn’t use any external programs, making it a suitable
dependency for smaller libraries.


## Typical workflow

Normally, map file should be created by package maintainer.  On
end-user machine, necessary values from this pregenerated file are
retrieved.  If only relatively few values are required, a lot of
memory (and probably also CPU time) can be saved by not loading the
whole available database into memory on the user machine.

For a usage example, refer to package `datetime`.


## Creating a map

Creating disk maps doesn’t involve any additional tools and can be
done from Elisp.  Here is a trivial example:

    (extmap-from-alist "mycool.extmap"
                       '((foo . 42)
                         (bar . (we (can (store [almost "anything"]))))))

By default, function refuses to work if the output file already
exists.  However, you can tell it to overwrite it instead:

    (extmap-from-alist ... :overwrite t)


## Using a map

Using a map is also easy.  First, you need to initialize it:

    (extmap-init "mycool.extmap")

Keep in mind that the file must exist for as long as you use the map:
the whole point is that it loads missing data on-demand.

Now that you have map object, you can retrieve values from it:

    (let ((extmap (extmap-init "mycool.extmap")))
      (extmap-get extmap 'bar))

You don’t need to care if a value is already in memory or not: the map
will load it for you when necessary.


## Other functions

All the functions in the package are documented within Emacs (the
three demonstrated above also have more information).  Here is just a
full list:

* Using maps:
  - `extmap-init`
  - `extmap-get`
  - `extmap-contains-key`
  - `extmap-value-loaded`
  - `extmap-keys`
  - `extmap-mapc`
  - `extmap-mapcar`
  - `extmap-statistics`

* Creating maps:
  - `extmap-from-alist`
  - `extmap-from-iterator`
