;;; init.el -*- lexical-binding: t; -*-

;; TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW

(doom! :input
       japanese

       :completion
       (corfu +orderless +icons)
       (vertico +icons)

       :ui
       doom
       doom-dashboard
       (emoji +unicode)
       hl-todo
       modeline
       nav-flash
       ophints
       (popup +defaults)
       (vc-gutter +pretty)

       :editor
       (evil +everywhere)
       file-templates
       (format +onsave)
       snippets
       word-wrap

       :emacs
       (dired +dirvish)
       electric
       undo
       vc

       :term
       eshell
       vterm

       :checkers
       (syntax +flymake +icons)
       (spell +aspell +flyspell)

       :tools
       debugger
       direnv
       (eval +overlay)
       lookup
       (lsp +eglot)
       magit
       tree-sitter

       :os
       (:if (featurep :system 'macos) macos)
       (tty +osc)

       :lang
       (cc +tree-sitter)
       (clojure +tree-sitter)
       emacs-lisp
       (markdown +grip)
       (nix +tree-sitter)
       (org +pretty)
       (python +tree-sitter +lsp)
       (rust +lsp +tree-sitter)
       sh
       (yaml +tree-sitter)
       (zig +lsp +tree-sitter)

       :app
       calendar

       :config
       (default +bindings +smartparens))
