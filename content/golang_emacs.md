Title: Emacs + GoLang [gofmt not found problem]
Date: 2014-02-21
Tags: emacs, golang
Authors:  Alexander Kuleshov

Some time ago, as you maybe saw from my [tweet](https://twitter.com/0xAX/status/478185552194203650/photo/1) that I eventually back to emacs. i moved back from sublime text editor to emacs for go, erlang, js programming and general usage. I installed standard go-mode with emacs package manager, configured go-mode:

```elisp
(require 'go-mode)
```

and opened simple golang project in it. After the editing first *.go file i got a problem. When i tried to save my *.go file, i got error in emacs minibuffer:

```
Error: gofmt program not found.
```

Solution for this problem proved to be very simple:

```elisp
(setq load-path (cons "/home/alex/dev/go-projects/go/misc/emacs" load-path))
(require 'go-mode-load)
```
