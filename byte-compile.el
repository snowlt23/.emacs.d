(package-initialize)
(add-to-list 'load-path "~/.emacs.d/elisp")
(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)
(byte-recompile-directory (expand-file-name "~/.emacs.d/elisp") 0)
