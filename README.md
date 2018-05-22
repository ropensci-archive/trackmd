<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Project Status: WIP - Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)

trackmd
=======

RStudio addin for tracking changes in Markdown format. Inspired by
[Critic Markup](http://criticmarkup.com/).

Created at the RopenSci unconference. [Original
Issue](https://github.com/ropensci/unconf18/issues/76)

Main feature: `trackmd::trackChanges()`
=======================================

How to use:

1.  Open up the .Rmd or .md you want to edit.
2.  Run `trackmd::trackChanges(file)` where `file` is a character with
    the path of the file you just opened.
3.  Make some changes in the file.
4.  Save the file.
5.  See your changes in the viewer!
6.  Run `servr::daemon_stop()`, then click in the .Rmd or .md you edited
    to see the marked up file with changes.

![](trackchanges.gif)

Future
======

-   \[ \] A little buggy. Fix those.
-   \[ \] "Smarter" coloring: word by word instead of character by
    character?
-   \[ \] Addins to turn track changes on and off
-   \[ \] Accept / reject changes
-   \[ \] Communicate with track changes and comments in .docx files
    (Ultimate goal of original issue! Collaborate seamlessly with Word
    users.)
