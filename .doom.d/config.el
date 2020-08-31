;;; .doom.d/config.el -*- lexical-binding: t; -*-

;;; Theme and Fonts -------------------------------------
(setq doom-theme 'doom-palenight)

(setq doom-font (font-spec :family "Fira Code" :size 16)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 12)
      doom-big-font-mode nil
      doom-big-font-increment 2)

;; Bold keywords
(set-face-attribute 'font-lock-keyword-face nil :weight 'bold)

;; show unicode glyphs in different font
(set-fontset-font t 'unicode "all-the-icons" nil 'prepend)

;; unprettify prettified symbols when under cursor
(setq prettify-symbols-unprettify-at-point 'right-edge)

;; Ligatures in Fira Code
(global-fira-code-mode)
(setq fira-code-mode-disabled-ligatures '("x" "[]"))


;;; Keybindings ----------------------------------------
;; General
(define-key prog-mode-map (kbd "C-c f")
  'format-buffer)

(map! :leader
      :desc "Raise popup"
      "w m p" #'+popup/raise)

;; Haskell
(map! :map haskell-mode-map
      :localleader
      (:prefix-map ("l" . "lookup on hoogle")
       :desc "lookup word" "w" #'hoogle-word-under-caret
       :desc "lookup region" "r" #'hoogle-region)
      (:prefix-map ("r" . "run haskell process")
       :desc "load file" "r" #'haskell-process-load-file)
      :desc "switch to repl" "s" #'haskell-interactive-switch)

(map! :map interactive-haskell-mode-map
      :localleader
      :desc "switch to code" "s" #'haskell-interactive-switch-back)

;; org
(add-hook! org-mode
  (lambda ()
    (local-set-key (kbd "<C-return>") 'org-insert-heading)))

;;; -----------------------------------------------------

;;; evil ------------------------------------------------

;; o/O should not continue commented lines
(setq +evil-want-o/O-to-continue-comments nil)

;;; org-mode --------------------------------------------
;;Visual Stuff
(add-hook! org-mode (lambda ()
                      (org-bullets-mode t)
                      (yas-minor-mode t)))
;; Do not prettify symbols
(setq org-pretty-entities nil)

(setq org-ellipsis "⤵")

;;; -----------------------------------------------------

;;; ivy -------------------------------------------------
;; enable fuzzy matching in ivy
(setq ivy-re-builders-alist
      '((t . ivy--regex-plus)))
;;; -----------------------------------------------------

;;; treemacs

;; Do not show files ignored by git in treemacs
(with-eval-after-load 'treemacs
  (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?))
;;; -----------------------------------------------------

;;; company
(setq company-tooltip-idle-delay 0.2)
(setq company-idle-delay 0.2)
(setq company-show-numbers t)
;;; -----------------------------------------------------

;;; recentf

;; Do not show files in the .emacs.d directory in recent files
(with-eval-after-load 'recentf
  (add-to-list 'recentf-exclude
               (expand-file-name "~/.emacs.d//")))
;;; -----------------------------------------------------

;;; Haskell-Mode
(setq haskell-process-type 'stack-ghci
      haskell-process-path-stack "stack"
      haskell-process-args-stack-ghci '("--ghci-options=-ferror-spans"))

(setq haskell-interactive-popup-errors nil)

(defun hoogle-word-under-caret ()
  "Lookup the word under the caret on hoogle"
  (interactive)
  (save-excursion
    (forward-word)
    (let ((end (point)))
      (backward-word)
      (hoogle-region (point) end))))

(defun hoogle-region (start end)
  "Lookup the region between start and end on hoogle"
  (interactive "r")
  (haskell-hoogle (buffer-substring start end)))

;;; Utilities -------------------------------------------
(defun format-buffer ()
  "Format the whole buffer"
  (interactive)
  (set-face-attribute 'font-lock-keyword-face nil :weight 'bold)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
;;; -----------------------------------------------------
