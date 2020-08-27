;;; .doom.d/config.el -*- lexical-binding: t; -*-
;;; My personal emacs config
;;;
;;; Theme and Fonts -------------------------------------
(setq doom-theme 'doom-gruvbox)
(setq default-frame-alist '((font . "Fira Code 12")
                            (tool-bar-lines . 0)))

(global-fira-code-mode)
;; Pretty symbols
(set-face-attribute 'font-lock-keyword-face nil :weight 'bold)
(set-fontset-font t 'unicode "all-the-icons" nil 'prepend)
(setq prettify-symbols-unprettify-at-point 'right-edge)

(setq fira-code-mode-disabled-ligatures '("x" "[]"))
(global-hl-line-mode nil)

;;; Mac specific ----------------------------------------
(setq ns-right-alternate-modifier nil)
;;; -----------------------------------------------------

;;; Keybindings ----------------------------------------
;; General
(define-key prog-mode-map (kbd "C-c f")
  'format-buffer)

;; Lisp
(define-key emacs-lisp-mode-map (kbd "C-,") 'parinfer-toggle-mode)
(define-key lisp-mode-map (kbd "C-,") 'parinfer-toggle-mode)

;; org
(add-hook! org-mode (lambda ()
                      (local-set-key (kbd "<C-return>") 'org-insert-heading)
                      (org-bullets-mode)))

;;; -----------------------------------------------------

;;; org-mode --------------------------------------------
;;Visual Stuff
(add-hook! org-mode (lambda ()
                      (org-bullets-mode t)
                      (yas-minor-mode t)))
(setq org-pretty-entities t)
(setq org-ellipsis "⤵")

;;; -----------------------------------------------------

;;; ivy -------------------------------------------------
;; Fuzzy Matching:
(setq ivy-re-builders-alist
      '((t . ivy--regex-plus)))
;;; -----------------------------------------------------

;;; treemacs
(with-eval-after-load 'treemacs
  (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?))
;;; -----------------------------------------------------

;;; company
(setq company-tooltip-idle-delay 0.2)
(setq company-idle-delay 0.2)
(setq company-show-numbers t)
;;; -----------------------------------------------------

;;; recentf
(with-eval-after-load 'recentf
  (add-to-list 'recentf-exclude
               (expand-file-name "~/.emacs.d//")))
;;; -----------------------------------------------------

;;; lisp ------------------------------------------------
(add-hook 'emacs-lisp-mode-hook #'my-lisp-hook)

(setq clojure-indent-style 'always-indent)
(add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
(add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
(add-hook 'clojure-mode-hook (lambda () (local-set-key (kbd "TAB") #'cider-repl-indent-and-complete-symbol)
                               (setq company-idle-delay nil)))

(defun my-lisp-hook ()
  "Executed when entering lisp-mode"
  (mapc (lambda (pair) (push pair prettify-symbols-alist))
        '(("lambda" . "𝝺")
          ("->" . (decode-char 'ucs #XE112)))))

;; Setup for parinfer
;; https://shaunlebron.github.io/parinfer/

(setq parinfer-extensions
      '(defaults       ; should be included.
         pretty-parens  ; different paren styles for different modes.
         evil           ; If you use Evil.
         paredit        ; Introduce some paredit commands.
         smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
         smart-yank))   ; Yank behavior depend on mode.



;; Automatically switch to Indent Mode, where parens are dimmed
(setq parinfer-auto-switch-indent-mode nil)

;;
;;; -----------------------------------------------------

;;; Utilities -------------------------------------------
(defun format-buffer ()
  "Format the whole buffer"
  (interactive)
  (set-face-attribute 'font-lock-keyword-face nil :weight 'bold)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
;;; -----------------------------------------------------

