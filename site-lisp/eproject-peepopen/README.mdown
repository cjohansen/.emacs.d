# eproject-peepopen

## About

eproject-peepopen.el is a light-weight alternative to TopFunky's peepopen.el (https://github.com/topfunky/PeepOpen-EditorSupport), which depends on the presence of textmate.el for basic functionality.

eproject-peepopen uses jrockway's eproject.el to provide the same PeepOpen integration without introducing any of textmate.el's extraneous functionality. 

## Prerequisites

If you don't already have it, install eproject.el (https://github.com/jrockway/eproject)

## Installation

1. cd ~/.emacs.d/vendor (or your local equivalent)
1. git clone git://github.com/mbarnett/eproject-peepopen
1. Add the following to your emacs config file:

    (add-to-list 'load-path "~/.emacs.d/vendor/eproject-peepopen")

    (require 'eproject-peepopen)