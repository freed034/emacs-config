;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prefer-coding-system 'utf-8-unix) ;; set up coding system utf8
(global-font-lock-mode t) ;; get color to emacs
(setq ring-bell-function 'ignore) ;; disable the bell ring in emacs
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1)) ;; menu bar desactived
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ;; tool bar desactived
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ;; scroll bar desactived
(if (display-graphic-p) (progn (tool-bar-mode -1) (scroll-bar-mode -1))) ;; turn off the scroll bar and tool bar
(setq inhibit-splash-screen t) ;; disable the splash screen
(setq inhibit-startup-message t) ;; disable the startup message
(setq initial-scratch-message ";; This buffer is for text that is not saved, and for Lisp evaluation.\n\n") ;; customize message for scratch
;;(switch-to-buffer "*scratch*")

(setq default-frame-alist '((vertical-scroll-bars . nil) (font . "SFMono Nerd Font Mono-11")))
;;(add-to-list 'default-frame-alist '(font . "Monospace-12"))
;;(set-default-font "DejaVu Sans Mono 09")
;;(set-frame-font "Inconsolata 14" nil t)
;;(set-face-attribute 'default nil :height 100)

;; (setq window-divider-default-places t)
;; (setq window-divider-default-right-width 1)
;; (setq window-divider-default-bottom-width 1)
;; (add-hook 'windows-setup-hook #'window-divider-mode)

(require 'iso-transl) ;; set up for use the acents, included the circonflex accent for latex math mode
(setq-default indent-tabs-mode nil) ;; disable the use of tabs
;;(require 'fill-column-indicator) ;; load fill-column-indicator mode

;;(add-hook 'text-mode-hook 'auto-fill-mode) ;; set 80 characters for line
(add-hook 'text-mode-hook 'turn-on-visual-line-mode) ;; add the line wrap for all text modes
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)) ;; the fringe to mark wrapped lines

;;(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
;;(global-fci-mode t)

(fset 'yes-or-no-p 'y-or-n-p) ;; y/n instead of yes/no
(global-auto-revert-mode t) ;; auto revert file on change
(setq vc-follow-symlinks t) ;; set by default the option yes for follow links

(normal-erase-is-backspace-mode 0) ;; use backspace in a correct way in a keyboard with delete
(setq x-select-enable-clipboard t) ;; enable system copy with emacs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backup files in /tmp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq backup-directory-alist `((".*" . ,temporary-file-directory)))

;; this line is for auto-save
;; (setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tramp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set default connection mode to SSH
(setq tramp-default-method "ssh")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Which-key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package which-key
  :init (which-key-mode t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows numbering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package winum
  :ensure t
  :init (winum-mode t)
  ;; :config (winum-set-keymap-prefix (kbd "\C-c")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exec-path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (exec-path-from-shell-copy-env "LC_ALL")
;; (exec-path-from-shell-copy-env "LAN")
;; (exec-path-from-shell-copy-env "LANGUAGE")
;; (exec-path-from-shell-copy-env "XDG_CURRENT_DESKTOP")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the lang variable for force the use of english on emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For use english as primary language
(setf (getenv "LC_ALL") "en_US.UTF-8")
(setf (getenv "LANG") "en_US.UTF-8")
(setf (getenv "LANGUAGE") "en_US.UTF-8")

;; For set up the desktop system
(setf (getenv "XDG_CURRENT_DESKTOP") "XFCE")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (E)Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'lisp-mode-hook 'display-line-numbers-mode)
(add-hook 'emacs-lisp-mode-hook 'display-line-numbers-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linum (line numbers)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (global-linum-mode t) ;; enable line numbers globally
;; (setq linum-format "%4d ")
;; ;;(setq linum-format "%d ") ;; numbers begin in the left
;; ;;(setq linum-format "%4d \u2502")
;; (setq linum-disabled-modes-list '(mu4e-headers-mode
;;                                   mu4e-view-mode
;;                                   mu4e-main-mode
;;                                   mu4e~update-mail-mode
;;                                   term-mode
;;                                   term-line-mode
;;                                   term-char-mode
;;                                   wl-summary-mode
;;                                   compilation-mode
;;                                   doc-view-mode
;;                                   pdf-view-mode
;;                                   image-mode
;;                                   dired-mode
;;                                   eshell-mode
;;                                   elfeed-search-mode
;;                                   elfeed-show-mode
;;                                   treemacs-mode
;;                                   sage-shell-mode
;;                                   arxiv-mode
;;                                   arxiv-abstract-mode
;;                                   org-mode
;;                                   org-agenda-mode
;;                                   slime-mode
;;                                   locate-mode))

;; (defun freed034/linum-on () (unless (or (minibufferp) (member major-mode linum-disabled-modes-list)) (linum-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sublimity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (sublimity-mode t)
;; (require 'sublimity)
;; (require 'sublimity-scroll)
;; (require 'sublimity-map) ;; experimental
;; (require 'sublimity-attractive)

;; (setq sublimity-scroll-weight 10
;;      sublimity-scroll-drift-length 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Left-ALT-as-Meta, Right-Alt-for-Special-Characters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(setq ns-alternate-modifier 'meta)
;;(setq ns-right-alternate-modifier 'none)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Function to load my org file TODOs_personal
;; (defun freed034/load-personal ()
;;   (interactive)
;;   (find-file "org/personal.org"))

;; ;; Function to load my org file TODOs_work
;; (defun freed034/load-work ()
;;   (interactive)
;;   (find-file "org/work.org"))

;; Function to desactive linum mode when i am in org-mode
;;(defun freed034/nolinum ()
;;  (interactive)
;;  (message "Desactivated linum mode")
;;  (global-linum-mode 0)
;;  (linum-mode 0)
;;)

;; New function my-compile-hook
;;(defun freed034/my-compile-hook () (interactive) ())
;;(add-hook 'LaTeX-mode-hook 'freed034/my-compile-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define new shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Shortcut for org notes
;; (global-set-key (kbd "\C-c w") 'load-work)
;; (global-set-key (kbd "\C-c p") 'load-personal)

;; Shortcut for undo
(global-set-key (kbd "\C-z") 'undo)

;; Shortcut for compile in TeX
(add-hook 'LaTeX-mode-hook (lambda () (define-key LaTeX-mode-map (kbd "\C-c \C-a") 'TeX-command-run-all)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(require 'helm)
(helm-mode t)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode t)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change helm-command-prefix-key once helm-config is loaded.
(global-set-key (kbd "\C-c h") 'helm-command-prefix)
(global-unset-key (kbd "\C-x c"))
(global-set-key (kbd "\C-c h a") 'helm-ag)

(global-set-key (kbd "\C-x f") #'helm-find)
(global-set-key (kbd "\C-x \C-f") #'helm-find-files)
(global-set-key (kbd "\C-x \C-r") #'helm-recentf)
(global-set-key (kbd "\C-x b") #'helm-mini)
(global-set-key (kbd "\C-x \C-b") #'buffer-menu-other-window)
;; (global-set-key (kbd "\C-x \C-b") #'helm-buffers-list)
(global-set-key (kbd "\M-x") #'helm-M-x)
(global-set-key (kbd "\M-y") #'helm-show-kill-ring)
(global-set-key (kbd "\C-x r l") #'helm-filtered-bookmarks)
(global-set-key (kbd "\C-x a") 'helm-list-elisp-packages)
(global-set-key (kbd "\C-x c!") 'helm-calcul-expression)
(global-set-key (kbd "\C-x c:") 'helm-eval-expression-with-eldoc)
;; (global-set-key (kbd "M-s o") #'helm-swoop)
;; (global-set-key (kbd "M-s /") #'helm-multi-swoop)

(helm-adaptive-mode t)

;;(define-key org-mode-map (kbd "\C-x c o h") #'helm-org-headlines)
(define-key helm-map (kbd "\M-o") 'helm-previous-source)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ;; rebind tab to run persistent action
(define-key helm-map (kbd "\C-i") 'helm-execute-persistent-action) ;; make TAB work in terminal
(define-key helm-map (kbd "\C-z") 'helm-select-action) ;; list actions using C-z

(setq helm-split-window-in-side-p           t ;; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ;; nil ;; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ;; search for library in require and declare-function sexp.
      helm-scroll-amount                    8 ;; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line        t)

;;(add-hook 'helm-minibuffer-set-up-hook
;;          'spacemacs//helm-hide-minibuffer-maybe)

;; (setq helm-display-function 'helm-display-buffer-in-own-frame
;;         helm-display-buffer-reuse-frame t
;;         helm-use-undecorated-frame-option t)

;; list of the buffers that I don't want to see
(setq helm-boring-buffer-regexp-list (list (rx "*magit-")
                                           (rx ".git")
                                           (rx "*helm")
                                           (rx "*Mini")
                                           (rx "*Echo")
                                           (rx "*mu4e")
                                           (rx "*scratch")
                                           (rx "*Completions")
                                           (rx "*http melpa.org:80*")
                                           (rx "*temp*")
                                           (rx "*autoload*")
                                           (rx "*code-conversion-work*")
                                           (rx "*code-converting-work*")
                                           (rx "*tip*")))

;;(require 'helm-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ido mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (setq ido-ignore-extensions t) ;; completation-ignored-extensions
;; (ido-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun freed034/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; ;; Set faces for heading levels
  ;; (dolist (face '((org-level-1 . 1.2)
  ;;                 (org-level-2 . 1.1)
  ;;                 (org-level-3 . 1.05)
  ;;                 (org-level-4 . 1.0)
  ;;                 (org-level-5 . 1.1)
  ;;                 (org-level-6 . 1.1)
  ;;                 (org-level-7 . 1.1)
  ;;                 (org-level-8 . 1.1)))
  ;;   (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  ;; (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  ;; (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  ;; (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  ;; (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  ;; (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  ;; (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  ;; (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)
  )

(defun freed034/org-mode-setup ()
  (org-indent-mode t)
  ;; (variable-pitch-mode t)
  ;; (visual-line-mode t)
  )

(use-package org
  ;; :pin org
  :commands (org-capture org-agenda)
  :hook (org-mode . freed034/org-mode-setup)
  :config (progn
            (setq org-directory user-ORG)
            (setq org-ellipsis " ▾")
            (setq org-log-done 'time)
            ;;(setq org-log-done 'note)
            (setq org-agenda-window-setup (quote current-window)) ;; Open agenda in current window
            (define-key global-map (kbd "\C-c a") 'org-agenda) ;; Set key for agenda
            ;;(setq org-list-description-max-indent 5) ;; Set maximum indentation for description lists
            ;;(setq org-adapt-indentation nil) ;; Prevent demoting heading also shifting text inside sections
            (setq org-agenda-files (directory-files-recursively user-ORG "\\.org$")) ;; Files to save todo items
  
            ;; Adding new workflow states for every org-mode file
            ;; that is the same to write
            ;; #+TODO: NEXT TODO IN-PROGRESS WAITING SOMEDAY PROJECT DONE CANCELLED
            ;; in the top of every org-mode file
            (setq org-todo-keywords
	          '((sequence "NEXT(n)" "TODO(t)" "IN-PROGRESS(i)" "WAITING(w@/!)" "|" "DONE(d)")
	            (sequence "IDEA" "|")
                    (sequence "PROJECT(j)" "|")
                    (sequence "BIRTHDAY" "DEATHDAY" "|")
                    (sequence "OPEN(o)" "|" "CLOSED")
                    (sequence "PLANNED(p)" "GOING(g)" "|" "ASSISTED(a)" "NOT-ASSISTED" "MISSING")
	            (sequence "ZOOM(z)" "BBB(b)" "SKYPE(k)" "|" "SEEN(s)" "UNSEEN(u)")
                    (sequence "|" "POSTPONED(p)" "CANCELED(c@)")))

            ;; Save Org buffers after refiling!
            (advice-add 'org-refile :after 'org-save-all-org-buffers)
            
            ;; Set priority range from A to C with default A
            (setq org-highest-priority ?A
	          org-lowest-priority ?C
	          org-default-priority ?A)
            
            ;; Set colours for priorities
            (setq org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold))
                                       (?B . (:foreground "LightSteelBlue"))
                                       (?C . (:foreground "OliveDrab"))))
            
            (setq org-tag-alist '(("@notes"     . ?n)
			          ("@talk"      . ?t)
			          ("@gdt"       . ?g)
			          ("@paper"     . ?p)))

            (setq org-capture-templates '(("t"                ; key
                                           "ToDo"             ; description
                                           entry              ; type
                                           (file+headline (concat user-ORG-RES "quick.org") "Tasks") ; target
                                           "* TODO %^{Todo} %^g\n:PROPERTIES:\n:Created: %T\n:File: %f\n:Link: %a\n:About: %?\n:END:\n" ; template
                                           :prepend t        ; properties
                                           :empty-lines 1    ; properties
                                           :created t        ; properties
                                           )
                                          ("i"                ; key
                                           "Idea"             ; description
                                           entry              ; type
                                           (file+headline (concat user-ORG-RES "quick.org") "Ideas") ; target
                                           "* IDEA %^{Idea} %^g\n:PROPERTIES:\n:Created: %T\n:File: %f\n:Link: %a\n:About: %?\n:END:\n" ; template
                                           :prepend t        ; properties
                                           :empty-lines 1    ; properties
                                           :created t        ; properties
                                           )
                                          ("s"                ; key
                                           "Source"         ; description
                                           entry              ; type
                                           (file+headline (concat user-ORG-RES "quick.org") "Sources") ; target
                                           "* SOURCE %^{Source} %^g\n:PROPERTIES:\n:Created: %T\n:File: %f\n:Link: %a\n:About: %?\n:END:\n" ; template
                                           :prepend t        ; properties
                                           :empty-lines 1    ; properties
                                           :created t        ; properties
                                           )))

            ;; Capture todo items using C-c c
            ;;(define-key global-map (kbd "\C-c c") 'org-capture)
            (define-key global-map (kbd "\C-c j") (lambda () (interactive) (org-capture nil "jj")))
            
            ;;(define-key global-map (kbd "\C-cl") 'org-store-link)
            ;;(define-key global-map (kbd "\C-cb") 'org-ido)

            (freed034/org-font-setup))
  )

(defun freed034/org-mode-visual-fill ()
  (setq visual-fill-column-width 100)
  (setq visual-fill-column-center-text t)
  (visual-fill-column-mode t))

(use-package visual-fill-column
  :hook (org-mode . freed034/org-mode-visual-fill)
  )

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-roam
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-roam
  :ensure t
  :init (setq org-roam-v2-ack t)
  :custom (org-roam-directory (file-truename user-ROAM))
  :bind (("\C-c n f" . org-roam-node-find)
         ("\C-c n i" . org-roam-node-insert)
	 ("\C-c n r" . org-roam-node-random)
	 ("\C-c n c" . org-roam-capture)
	 ("\C-c n o" . org-id-get-create)
	 ("\C-c n t" . org-roam-tag-add)
	 ("\C-c n a" . org-roam-alias-add)
	 ("\C-c n g" . org-roam-graph)
	 ("\C-c n l" . org-roam-buffer-toggle))
  :config (progn
	    (org-roam-setup)
	    (setq org-roam-capture-templates '(("d" "default" plain "%?"
				                :if-new (file+head "org-notes/%<%Y%m%d%H%M%S>-${slug}.org"
						                   "#+TITLE: ${title}\n#+FILETAGS: :notes:\n#+DATE: %u\n#+LASTMOD: \n\n")
				                :immediate-finish t
                                                :unnarrowed t)
                                               ;; ("r" "bibliography note" plain "%?"
				               ;;  :if-new (file+head "org-references/${citekey}.org"
				               ;; 		       "#+TITLE: ${title}\n#+DATE: %u\n#+LASTMOD: \n\n")
				               ;;  :immediate-finish t
				               ;;  :unnarrowe t)
                                               )
		  time-stamp-start "#\\+LASTMOD: [\t]*"))
  )

(use-package org-roam-bibtex
  :after (org-roam helm-bibtex)
  :bind (:map org-mode-map ("\C-c n b" . orb-note-actions))
  :hook (org-roam-mode org-roam-bibtex-mode)
  :config (progn
            (require 'org-ref)
            (setq  orb-insert-interface 'helm-bibtex
		   orb-preformat-keywords '("=key=" "title" "url" "author-or-editor" "keywords" "file")
		   orb-process-file-field t
		   orb-process-file-keyword t
		   orb-file-field-extensions '("pdf")))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-babel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun freed034/org-insert-src-block (src-code-type)
  "Insert a `src-code-type' type source code block in org-mode"
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "lisp" "org"  "latex" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen" "octave" "oz" "plantuml"
            "R" "sass" "screen" "sql" "awk" "ditaa" "haskell" "matlab" "ocaml" "perl" "ruby" "scheme" "sqlite")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+begin_src %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+end_src\n")
    (previous-line 2)
    (org-edit-src-code)))

(add-hook 'org-mode-hook #'(lambda ()
                             ;; turn on flyspell-mode by default
                             ;; (flyspell-mode 1)
                             ;; C-TAB for expanding
                             (local-set-key (kbd "C-<tab>")
                                            'yas/expand-from-trigger-key)
                             ;; keybinding for editing source code blocks
                             (local-set-key (kbd "C-c s e")
                                           'org-edit-src-code)
                             ;; keybinding for inserting code blocks
                             (local-set-key (kbd "C-c s i")
                                            'freed034/org-insert-src-block)
                             ))

(org-babel-do-load-languages 'org-babel-load-languages
                             '((emacs-lisp . t)
                               (shell  . t)
                               (latex . t)
                               ;; add another language here
                               ;; python & jupyter
                               (python . t)
                               (jupyter . t)
                               (R . t)
                               ))

(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)

(org-babel-jupyter-override-src-block "python")

(require 'ob-async)
(setq ob-async-no-async-languages-alist '("python" "jupyter-python"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'python-mode-hook 'display-line-numbers-mode)

;; Use ipython for REPL
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")

;; Use Jupiter for REPL
;; (setq python-shell-interpreter "jupyter"
;;       python-shell-interpreter-args "console --simple-prompt"
;;       python-shell-prompt-detect-failure-warning nil)
;; (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")

;; For python programming language support
(use-package elpy
  :ensure t
  :defer t
  :init (advice-add 'python-mode :before 'elpy-enable)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs speaks statistics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ess-site)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (projectile-mode t)
;; (define-key projectile-mode-map (kbd "\C-c p") 'projectile-command-map)
;; (define-key projectile-mode-map [?\C-d] 'projectile-find-dir)
;; (define-key projectile-mode-map [?\C-p] 'projectile-switch-project)
;; (define-key projectile-mode-map [?\C-f] 'projectile-find-file)
;; ;; (define-key projectile-mode-map [?\C-g] 'projectile-grep)
;; ;; (define-key projectile-mode-map (kbd "\C-c p /") #'(lambda () (interactive) (helm-ag (projectile-project-root))))
;; (setq projectile-auto-discover nil)
;; (setq projectile-project-search-path '(user-RES))

;; ;; turns on projectile mode by default for all file types
;; ;; (require 'projectile)
;; ;; (projectile-global-mode t)
;; ;; (setq projectile-enable-caching t)
;; ;; (setq projectile-keymap-prefix (kbd "\C-c p"))
;; ;; (setq projectile-switch-project-action 'helm-projectile-find-file) ;; asks for file to open when project is switched

;; ;; (use-package helm-projectile
;; ;;   :config (progn
;; ;;             (setq helm-projectile-sources-list (cons 'helm-source-projectile-files-list
;; ;;                                                    (remove 'helm-source-projectile-files-list
;; ;;                                                            helm-projectile-sources-list)))
;; ;;             (helm-projectile-on))
;; ;;   )

;; (use-package org-projectile
;;   :ensure t
;;   :bind (("\C-c n p" . org-projectile-project-todo-completing-read)
;;          ("\C-c c" . org-capture))
;;   :config (progn
;;             (setq org-projectile-projects-file (concat user-RES "TODOs.org"))
;;             ;;(setq org-projectile-capture-template (format ))
;;             (add-to-list 'org-capture-templates
;;                          (org-projectile-project-todo-entry
;;                           :capture-template "* TODO %^{Todo} %^g\n:PROPERTIES:\n:Created: %T\n:File: %f\n:Link: %a\n:About: %?:END:\n"
;;                           :capture-character "t2"
;;                           :capture-heading "Linked Project TODO"))
;;             ;; (add-to-list 'org-capture-templates
;;             ;;              (org-projectile-project-todo-entry
;;             ;;               :capture-template "* IDEA %^{Idea} %^g\n:PROPERTIES:\n:Created: %T\n:File: %f\n:Link: %a\n:About: %?:END:\n"
;;             ;;               :capture-character "i2"))
;;             (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files))))
;;   )

;; ;; (require 'org-projectile)
;; ;; (org-projectile-per-project)
;; ;; (setq org-projectile-projects-file (concat user-RES "TODOs.org"))
;; ;; (push (org-projectile-project-todo-entry) org-capture-templates)
;; ;; ;;(global-set-key (kbd "\C-c c") 'org-capture)
;; ;; (global-set-key (kbd "\C-c n p") 'org-projectile-project-todo-completing-read)
;; ;; (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Treemacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package treemacs
  :ensure t
  :init (with-eval-after-load 'winum (define-key winum-keymap (kbd "\M->") #'treemacs-select-window))
  :bind (:map global-map
              ("\C-x t t"   . treemacs)
              ("\C-x t s"       . treemacs-select-window)
              ("\C-x t 1"   . treemacs-delete-other-windows)
	      ;;("\C-x t \C-t" . treemacs-find-file)
              ;;("\C-x t \M-t" . treemacs-find-tag)
              ("\C-x t p"   . treemacs-projectile)
              ("\C-x t B"   . treemacs-bookmark))
  :config (progn
	    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
		  treemacs-deferred-git-apply-delay   0.5
		  treemacs-display-in-side-window     t
		  treemacs-file-event-delay           5000
		  treemacs-file-follow-delay          0.2
		  treemacs-follow-after-init          t
		  treemacs-follow-recenter-distance   0.1
		  treemacs-goto-tag-strategy          'refetch-index
		  treemacs-indentation                1
		  treemacs-indentation-string         " " ;; (propertize " ⫶ " 'face 'font-lock-comment-face)
		  treemacs-is-never-other-window      nil
		  treemacs-no-png-images              nil
		  treemacs-project-follow-cleanup     nil
		  treemacs-persist-file               (expand-file-name "treemacs-persist" user-emacs-directory)
		  treemacs-position                   'right
		  treemacs-recenter-after-file-follow nil
		  treemacs-recenter-after-tag-follow  nil
		  treemacs-show-hidden-files          nil
		  treemacs-git-integration            t
		  treemacs-collapse-dirs              0
		  treemacs-follow-mode                t
		  treemacs-filewatch-mode             t
		  treemacs-fringe-indicator-mode      t
		  treemacs-silent-filewatch           nil
		  treemacs-silent-refresh             nil
		  treemacs-sorting                    'alphabetic-asc
		  treemacs-space-between-root-nodes   nil
		  treemacs-tag-follow-cleanup         t
		  treemacs-tag-follow-delay           1.5
		  ;;treemacs-resize-icons               44 ;; the default width and height of icons is 22 pixels. For Hi-DPI display, use 44.
		  treemacs-width                      25))
    ;; (pcase (cons (not (null (executable-find "git"))) (not (null (executable-find "python3"))))
    ;;   (`(t . t) (treemacs-git-mode 'extended))
    ;;   (`(t . _) (treemacs-git-mode 'simple)))
  )

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t
  )

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OpenWith
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package openwith
  :init (openwith-mode t)
  :config (setq openwith-associations '(;("\\.pdf\\'" "zathura" (file))
                                        ("\\.mp4\\'" "mpv" (file))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'auto-complete)
;; ;;(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (global-auto-complete-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight-indent-guides
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
;; (setq highlight-indent-guides-method 'character)
;; (setq highlight-indent-guides-caracter ?\|)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Todoist
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(require 'todoist)
;;(setq todoist-api-token "xxxxxxxxxxxx")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit (git)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :ensure t
  :bind (("\C-x g" . magit-status)
	 ("\C-x \C-g" . magit-dispatch-popup))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell script mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'shell-script-mode-hook 'display-line-numbers-mode)
(add-hook 'sh-mode-hook 'display-line-numbers-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flymake
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(require 'flymake)
;;(defun freed034/flymake-get-tex-args (file-name)
;;(list "pdflatex"
;;(list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spell-checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(dolist (hook '(text-mode-hook)) (add-hook hook (lambda () (flyspell-mode t))))

(setq flyspell-issue-message-flag nil)
(setq ispell-dictionary "en_US") ;;(setq ispell-dictionary "en_US,en_GB,fr_FR,es_ES")
(setq ispell-local-dictionary "en_US")
(setq ispell-program-name "aspell") ;; "hunspell"
(setq ispell-silently-savep t)
(setq ispell-list-command "--list")
(setq ispell-extra-args '("--sug-mode=ultra" "--mode=tex"))
(setq ispell-personal-dictionary (concat user-DIR "data/perso_en_dict"))

;; (add-hook 'LaTeX-mode-hook (lambda ()
;;                               (setq ispell-tex-skip-alists
;;                                     (list
;;                                      (append
;;                                       (car ispell-tex-skip-alists)
;;                                       '(("[^\\]\\$" . "[^\\]\\$")))
;;                                      (cadr ispell-tex-skip-alist)))))

;;(add-to-list 'ispell-skip-region-alist '("#\\+begin_src". "#\\+end_src"))

;; (cond
;;  ;; try hunspell at first
;;  ;; if hunspell does NOT exist, use aspell
;;  ((executable-find "hunspell")
;;   (setq ispell-program-name "hunspell")
;;   (setq ispell-local-dictionary "en_US,en_GB,fr_FR,es_ES")
;;   (ispell-set-spellchecker-params)
;;   (ispell-hunspell-add-multi-dic "de_DE,de_CH,en_GB,en_US")
;;   (setq ispell-extra-args '("--sug-mode=ultra" "--mode=tex"))
;;   (setq ispell-local-dictionary-alist
;;         ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
;;         ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
;;         '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))

;;  ((executable-find "aspell")
;;   (setq ispell-program-name "aspell")
;;   (setq ispell-local-dictionary "en_US")
;;   ;;Please note ispell-extra-args contains ACTUAL parameters passed to aspell
;;   ;;(setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))
;;   (setq ispell-extra-args '("--sug-mode=ultra" "--mode=tex"))))

;; The personal dictionary file has to exist, otherwise hunspell will silently not use it.
;;(unless (file-ex ists-p ispell-personal-dictionary) (write-region "" nil ispell-personal-dictionary nil 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Guess language for detect language buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'guess-language)

(setq guess-language-languages '(en fr es))
(setq guess-language-min-paragraph-length 35)
(setq guess-language-langcodes
  '((en . ("en_US" "English" "" "American English"))
    (de . ("fr_FR" "French" "" "French"))
    (de . ("es_ES" "Spanish" "" "Spanish"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Outline Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun freed034/turn-on-outline-minor-mode ()
  (outline-minor-mode t))

(add-hook 'LaTeX-mode-hook 'freed034/turn-on-outline-minor-mode)
(add-hook 'latex-mode-hook 'freed034/turn-on-outline-minor-mode)
(setq outline-minor-mode-prefix "\C-c \C-o") ; Or something else

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AucTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package tex
;;   :ensure auctex)

(require 'tex-site)

;; Only start server for pdf viewer when in latex mode
;;(add-hook 'LaTeX-mode-hook 'server-start) ;; this line is deprecated because I use emacs daemon
;;(setq TeX-PDF-mode t) ;; this line is equivalent to the next line
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

;; Standard emacs/latex config
(add-hook 'LaTeX-mode-hook (lambda ()
                             (setq TeX-auto-save t)
                             (setq TeX-parse-self t)
                             (setq TeX-save-query nil) ;; autosave before compiling
                             (setq-default TeX-master nil)))

;; automatic detection of master file
(defun freed034/guess-TeX-master (filename)
  "Guess the master file for FILENAME from currently open .tex files."
  (let ((candidate nil)
        (filename (file-name-nondirectory filename)))
    (save-excursion
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (let ((name (buffer-name))
                (file buffer-file-name))
            (if (and file (string-match "\\.tex$" file))
                (progn
                  (goto-char (point-min))
                  (if (re-search-forward (concat "\\\\input{" filename "}") nil t)
                      (setq candidate file))
                  (if (re-search-forward (concat "\\\\include{" (file-name-sans-extension filename) "}") nil t)
                      (setq candidate file))))))))
    (if candidate
        (message "TeX master document: %s" (file-name-nondirectory candidate)))
    candidate))

;; (add-hook 'LaTeX-mode-hook #'(lambda () (setq TeX-master (freed034/guess-TeX-master (buffer-file-name)))))
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'display-line-numbers-mode)
;;(add-hook 'LaTeX-mode-hook 'auto-fill-mode) ;; enable auto-fill mode, nice for text

(setq TeX-electric-sub-and-superscript t)
;;(setq LaTeX-electric-left-right-brace t)

;;;; Brackets for the math mode
;; (add-hook 'plain-TeX-mode-hook (lambda () (set (make-local-variable 'TeX-electric-math)
;; 			                       (cons "$" "$"))))
;; (add-hook 'LaTeX-mode-hook (lambda () (set (make-local-variable 'TeX-electric-math)
;; 			                   (cons "$" "$"))))
;; (add-hook 'LaTeX-mode-hook (lambda () (set (make-local-variable 'TeX-electric-math)
;; 			                   (cons "\\(" "\\)"))))

;; Enable synctex correlation
(add-hook 'LaTeX-mode-hook (lambda ()
                             (setq TeX-source-correlate-mode t)
                             (setq TeX-source-correlation-method 'synctex)
                             (setq TeX-source-correlate-start-server t)))

;; Enable synctex generation. Even though the command shows
;; as "latex" pdflatex is actually called
'(LaTeX-command "latex -synctex=1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-hook 'LaTeX-mode-hook (lambda ()
;;                            (push
;;                             '("Latexmk" "latexmk -gg -pdf %S" TeX-run-TeX nil t ;;latexmk -pdf -pvc %t" TeX-run-TeX nil t ;;
;;                               :help "Run latexmk on file")
;;                             TeX-command-list)))

;; (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "Latexmk")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Xelatex and nomenclature compilation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load "tex" '(add-to-list 'TeX-command-list
                                     '("XeLaTeX" "xelatex -synctex=1 --interaction=nonstopmode %s"
                                       TeX-run-command t t :help "Run xelatex on file") t))

(eval-after-load "tex" '(add-to-list 'TeX-command-list
                                     '("Nomenclature" "makeindex %s.nlo -s nomencl.ist -o %s.nls"
                                       (lambda (name command file)
                                         (TeX-run-compile name command file)
                                         (TeX-process-set-variable file 'TeX-command-next TeX-command-default))
                                       nil t :help "Create nomenclature file")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Okular, evince or zathura as viewer, enable source <-> PDF sync
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Add a PDF viewer for auctex
;; (setq TeX-view-program-list '(("PDF Viewer" "okular --unique %o#src:%n%b")))
;; (setq TeX-view-program-list '(("PDF Viewer" "evince --page-index=%(outpage) %o")))

;; ;; Set the default PDF viewer
;; (eval-after-load "tex" '(add-to-list 'TeX-view-program-selection
;;                                      '(output-pdf "PDF Viewer")))
(eval-after-load "tex" '(add-to-list 'TeX-view-program-selection
                                     '(output-pdf "Zathura")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pdf-tools as PDF viewer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-tools-install)
  :bind ("\C-c \C-g" . pdf-sync-forward-search)
  ;; :defer t
  :config (progn
             (setq-default pdf-view-display-size 'fit-page)
             (define-key pdf-view-mode-map (kbd "\C-s") 'isearch-forward)
             (setq mouse-wheel-follow-mouse t)
             (setq pdf-view-resize-factor 1.10))
  )

;; ;; Use pdf-tools with auctex
;; (add-hook 'LaTeX-mode-hook 'pdf-tools-install)

;; ;; Use pdf-tools to open PDF files
;; (setq TeX-view-program-list '(("PDF Tools" "TeX-pdf-tools-sync-view")))
;; (setq TeX-view-program-selection '((output-pdf "PDF Tools")))

;; ;; Update PDF buffers after successful LaTeX runs
;; (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

;; ;; Pdf-tools in another frame improved
;; (defun framesMenus-read-window (&optional prompt)
;;   "Prompt with PROMPT and read a window."
;;   (save-selected-window
;;     (let ((frame (selected-frame))
;;           window ev)
;;       (while (progn
;;                (setq ev (read-event prompt))
;;                (eq (event-basic-type ev) 'switch-frame))
;;         (setq frame (cadr ev)))
;;       (with-selected-frame frame
;;         (and
;;          (window-live-p
;;           (setq window (posn-window (event-start ev))))
;;          window
;;          )))))

;; (defvar-local framesMenus-associated-other-window nil
;;   "Alist of other windows associated with selected windows for a buffer.")

;; (defun framesMenus-associated-other-window (&optional prompt force mutual)
;;   "Return the other window associated with the current buffer.
;; PROMPT is forwarded to `framesMenus-read-window' when called.
;; When FORCE is non-nil always prompt for the associated window.
;; If MUTUAL is non-nil also associate other window with this one."
;;   (let* ((this-window (selected-window))
;;          (assoc-window (assoc this-window framesMenus-associated-other-window))
;;          (other-window (cdr assoc-window)))
;;     (unless
;;         (and
;;          (null force)
;;          (window-live-p other-window)
;;          other-window)
;;       (cl-pushnew
;;        (cons (selected-window)
;;              (setq other-window (framesMenus-read-window prompt)))
;;        framesMenus-associated-other-window
;;        :key #'car
;;        ))
;;     (when mutual
;;       (with-selected-window other-window
;;         (cl-pushnew
;;          (cons other-window
;;                this-window)
;;          framesMenus-associated-other-window
;;          :key #'car)))
;;     other-window))

;; (defun framesMenus-display-in-associated-window (buffer _action-list)
;;   "Display BUFFER in associated window."
;;   (let ((window (framesMenus-associated-other-window "Associate window." nil t)))
;;     (with-selected-window window
;;       (switch-to-buffer buffer))
;;     window))

;; (defun framesMenus-display-buffer-use-associated-window (fun &rest args)
;;   "Use `display-buffer-use-some-frame' as `display-buffer-overriding-action'.
;; Then run FUN with ARGS."
;;   (let ((display-buffer-overriding-action '(framesMenus-display-in-associated-window)))
;;     (apply fun args)))

;; (advice-add 'TeX-pdf-tools-sync-view :around #'framesMenus-display-buffer-use-associated-window)
;; (advice-add 'pdf-sync-backward-search-mouse :around #'framesMenus-display-buffer-use-associated-window)

;; ;; ;; Pdf-tools in another frame
;; ;; (defun freed034/framesMenus-display-buffer-use-some-frame (fun &rest args)
;; ;;   "Use `display-buffer-use-some-frame' as `display-buffer-overriding-action'. Then run FUN with ARGS."
;; ;;   (let ((display-buffer-overriding-action '(display-buffer-use-some-frame)))
;; ;;     (apply fun args)))

;; ;; (advice-add 'TeX-pdf-tools-sync-view :around #'freed034/framesMenus-display-buffer-use-some-frame)
;; ;; (advice-add 'pdf-sync-backward-search-mouse :around #'freed034/framesMenus-display-buffer-use-some-frame)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auctex-latexmk
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(require 'auctex-latexmk)
;;(auctex-latexmk-setup)
;;(setq auctex-latexmk-inherit-TeX-PDF-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reftex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase Mode" t)
;; (add-hook 'reftex-load-hook 'imenu-add-menubar-index)

(setq LaTeX-eqnarray-label "eq"
      LaTeX-equation-label "eq"
      LaTeX-figure-label "fig"
      LaTeX-table-label "tab"
      LaTeX-myChapter-label "chap"
      TeX-newline-function 'reindent-then-newline-and-indent
      LaTeX-section-hook '(LaTeX-section-heading
                           LaTeX-section-title
                           LaTeX-section-toc
                           LaTeX-section-section
                           LaTeX-section-label))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bibtex manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (global-set-key (kbd "\C-c e") 'ebib)
;; (add-hook 'bibtex-mode-hook 'flyspell-mode)
(setq bibtex-dialect 'biblatex)
(setq bibtex-user-optional-fields '(("keyboards" "Keywords to describe the entry" "")
                                    ("file" "Link to the document file" ":"))
      bibtex-align-at-equal-sign t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bibliography manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq pdf-files-directory (file-truename (concat user-ROAM "bibtex-pdfs"))
      bib-files-directory (file-truename (concat user-ROAM "bibtex-files"))
      org-files-directory (file-truename (concat user-ROAM "org-references")))
      
(use-package helm-bibtex
  :after helm
  :bind (("\C-c n B" . helm-bibtex)
	 ("\C-c n s" . bibtex-sort-buffer))
  :config (progn
            (setq bibtex-completion-library-path pdf-files-directory
                  bibtex-completion-bibliography (list (concat bib-files-directory "/other.bib")
                                                       (concat bib-files-directory "/arXiv.bib")
                                                       (concat bib-files-directory "/master.bib"))
                  bibtex-completion-notes-path org-files-directory
                  bibtex-completion-pdf-symbol "*";;"⌘"
                  bibtex-completion-notes-symbol "+";;"✎"
                  bibtex-completion-pdf-field "file"
                  ;; bibtex-completion-pdf-open-function (lambda (fpath) (call-process "zathura" nil 0 nil fpath))
                  bibtex-completion-browser-function (lambda (url _) (start-process "firefox" "*firefox*" "firefox" url))
                  bibtex-completion-additional-search-fields '(keywords)
                  bibtex-completion-notes-template-multiple-files (concat "#+TITLE: ${title}\n"
									  "#+ROAM_KEY: cite:${=key=}\n"
									  ":PROPERTIES:\n"
									  ":Custom_ID: ${=key=}\n"
									  ":NOTER_DOCUMENT: ${file}\n"
									  ":AUTHOR: ${author-abbrev}\n"
									  ":JOURNAL: ${journaltitle}\n"
									  ":DATE: ${date}\n"
									  ":YEAR: ${year}\n"
									  ":DOI: ${doi}\n"
									  ":URL: ${url}\n"
									  ":END:\n\n"
                                                                          "* ")
                  bibtex-completion-display-formats '((t . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:20} ${title:*}")))
  
            (advice-add 'bibtex-completion-candidates :filter-return 'reverse))
  )

(use-package org-ref
  :config (setq org-ref-completion-library 'org-ref-ivy-cite
		org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
		org-ref-notes-function 'orb-edit-notes
		org-ref-pdf-directory pdf-files-directory
		org-ref-default-bibliography (list (concat bib-files-directory "/other.bib")
                                                   (concat bib-files-directory "/arXiv.bib")
                                                   (concat bib-files-directory "/master.bib"))
		org-ref-notes-directory org-files-directory
		;; org-ref-bibliography-notes
		org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
		reftex-default-bibliography org-ref-default-bibliography
		bibtex-completion-library-path org-ref-pdf-directory
		bibtex-completion-bibliography org-ref-default-bibliography
		bibtex-completion-notes-path org-ref-notes-directory)
  )

(use-package org-noter
  :ensure t
  ;; :defer t
  :after (:any org pdf-tools)
  :config (setq org-noter-notes-window-location 'other-frame
		org-noter-always-create-frame nil
		org-noter-hide-other nil
		org-noter-notes-search-path (list org-files-directory))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired : open the current file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "\C-x d") (lambda () (interactive) (dired (getenv "HOME"))))
(with-eval-after-load 'dired (require 'dired-x))
(setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode t)))

;; (defun open-in-external-app (&optional @fname)
;;   "Open the current file or dired marked files in external app.
;;    The app is chosen from your OS's preference.
;;    When called in emacs lisp, if @fname is given, open that.
;;    Version 2019-01-18"
;;   (interactive)
;;   (let* (
;;          ($file-list
;;           (if @fname
;;               (progn (list @fname))
;;             (if (string-equal major-mode "dired-mode")
;;                 (dired-get-marked-files)
;;               (list (buffer-file-name)))))
;;          ($do-it-p (if (<= (length $file-list) 5)
;;                        t
;;                      (y-or-n-p "Open more than 5 files? "))))
;;     (when $do-it-p
;;       (cond
;;        ((string-equal system-type "windows-nt")
;;         (mapc
;;          (lambda ($fpath)
;;            (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" $fpath t t))) $file-list))
;;        ((string-equal system-type "darwin")
;;         (mapc
;;          (lambda ($fpath)
;;            (shell-command
;;             (concat "open " (shell-quote-argument $fpath))))  $file-list))
;;        ((string-equal system-type "gnu/linux")
;;         (mapc
;;          (lambda ($fpath)
;;            (let ((process-connection-type nil))
;;              (start-process "" nil "xdg-open" $fpath))) $file-list))
;;        ))))

(put 'dired-find-alternate-file 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elfeed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'elfeed)
(global-set-key (kbd "\C-x e") 'elfeed)
(setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory))
;; (setq elfeed-show-entry-switch 'display-buffer)
(setq-default elfeed-search-filter "@2-week-ago +unread")
(add-hook 'elfeed-search-mode-hook 'elfeed-update)
;; (setq elfeed-feeds '(("http://export.arxiv.org/api/query?search_query=cat:math.OA&start=0&max_results=300&sortBy=submittedDate&sortOrder=descending" math.OA)
;;                      ("http://export.arxiv.org/api/query?search_query=cat:math.QA&start=0&max_results=300&sortBy=submittedDate&sortOrder=descending" math.QA)))

(require 'elfeed-goodies)
(elfeed-goodies/setup)
(defalias 'elfeed-toggle-star (elfeed-expose #'elfeed-search-toggle-all 'star))
(eval-after-load 'elfeed-search '(define-key elfeed-search-mode-map (kbd "m") 'elfeed-toggle-star))

(require 'elfeed-org)
(elfeed-org)
(setq rmh-elfeed-org-files (list (concat user-DIR "data/elfeed.org")))

;; Elfeed functions
(defun freed034/elfeed-show-all ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-all"))
(defun freed034/elfeed-show-emacs ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-emacs"))
(defun freed034/elfeed-show-daily ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-daily"))

;; Elfeed functions to support syncing "~/.config/emacs/elfeed" between machines
;; makes sure elfeed reads index from disk before launching
(defun freed034/elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed)
  (elfeed-db-load)
  (elfeed-search-update--force)
  (elfeed-update))

;; makes sure elfeed write to disk when quiting
(defun freed034/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

;; Keybidings for the above functions
(define-key elfeed-search-mode-map (kbd "A") 'freed034/elfeed-show-all)
(define-key elfeed-search-mode-map (kbd "E") 'freed034/elfeed-show-emacs)
(define-key elfeed-search-mode-map (kbd "D") 'freed034/elfeed-show-daily)
(define-key elfeed-search-mode-map (kbd "q") 'freed034/elfeed-save-db-and-bury)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bookmarks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq bookmark-default-file (concat user-DIR "data/bookmarks")
;;       bookmark-save-flag 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell pop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package shell-pop
  :ensure t
  :bind ("\C-c x" . shell-pop)
  :config (progn
            (setq shell-pop-shell-type '("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell))))
            (setq shell-pop-term-shell "/bin/bash")
            (setq shell-pop-window-size 50)
            ;; (shell-pop-full-span t)
            ;; (shell-pop-window-position "bottom")
            ;; (shell-pop-default-directory (getenv "HOME"))
            ;; need to do this manually or not picked up by `shell-pop'
            (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight-focus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package highlight-focus
;;   :ensure nil
;;   :load-path (concat user-DIR "packages/highlight-focus/")
;;   :config (setq highlight-focus:face 'mode-line
;;                 highlight-focus:face-property :background
;;                 highlight-focus:face-property-value "darkblue")
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dim other buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'after-init-hook (lambda ()
                             (when (fboundp 'auto-dim-other-buffers-mode)
                               (auto-dim-other-buffers-mode t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Awesome-tab
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'awesome-tab)
;; (awesome-tab-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dimmer (visual highlight focus)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dimmer
  :ensure t
  :config (progn
	    (dimmer-configure-helm)
	    (dimmer-mode t)
	    ;; from 0 (nothing) to 1 (total)
	    (setq dimmer-fraction 0.5))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Doom-modeline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode t)
;;   :config (progn
;;             (setq doom-modeline-project-detection 'auto)
;;             ;;(setq doom-modeline-icon (display-graphic-p))
;;             (setq doom-modeline-height 15)
;;             ;; (set-face-attribute 'mode-line nil :family "Noto Sans" :height 100)
;;             ;; (set-face-attribute 'mode-line-inactive nil :family "Noto Sans" :height 100)
;;             (setq doom-modeline-bar-width 2)
;;             )
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Doom-themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ;; if nil, bold is universally disabled
        doom-themes-enable-italic t) ;; if nil, italics is universally disabled
  ;; Load the theme, keep in mind that each theme may have their own settings
  ;;(load-theme 'doom-one t)
  ;;(load-theme 'doom-one-light t)
  ;;(load-theme 'doom-city-lights t)
  (load-theme 'doom-outrun-electric t) ; nice
  ;;(load-theme 'doom-molokai t)
  ;;(load-theme 'doom-monokai-pro t)
  ;;(load-theme 'doom-vibrant t)
  ;;(load-theme 'doom-dracula t)
  ;;(load-theme 'doom-gruvbox t)
  ;;(load-theme 'doom-acario-dark t)
  ;;(load-theme 'doom-acario-light t)
  ;;(load-theme 'doom-dark+ t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;(let ((height (face-attribute 'default :height)))
  ;; for all linum/nlinum users
  ;;(set-face-attribute 'linum nil :height height))
  ;; only for `linum-relative' users:
  ;;(set-face-attribute 'linum-relative-current-face nil :height height)
  ;; only for `nlinum-relative' users:
  ;;(set-face-attribute 'nlinum-relative-current-face nil :height height))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package vscode-dark-plus-theme
;;   :config (load-theme 'vscode-dark-plus t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(use-package nyx-theme) ;; load nyx theme
;;(load-theme 'spacemacs-dark t) ;; load spacemacs theme in a dark version
;;(load-theme 'nimbus t) ;; load nimbus theme
;;(load-theme 'exotica t) ;; load exotica theme
;;(load-theme 'dracula t) ;; load dracula theme
;;(load-theme 'molokai) ;; load color theme molokai
;;(load-theme 'cobalt t) ;; load cobalt theme ;;(enable-theme 'cobalt)
;;(load-theme 'material t) ;; load material theme

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manual themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-to-list 'custom-theme-load-path (concat user-DIR "packages/themes/"))
;; (load-theme `tron-legacy t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All the icons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package all-the-icons
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; arXiv mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun freed034/arxiv-download-pdf (&optional confirm)
  "Download and save the highlighted paper to desired folder.
Return the file name of the saved pdf file.  You can change the
default folder by customizing the variable
`arxiv-default-download-folder'.  If CONFIRM is t, don't prompt the
user with opening file."
  (interactive)
  (let ((url (cdr (assoc 'pdf (nth arxiv-current-entry arxiv-entry-list))))
	(newfile) (pdfname) (input))
    (string-match "/\\([^/]+?\\)$" url)
    (setq pdfname (concat (match-string 1 url) ".pdf"))
    (setq newfile (read-file-name "save pdf as: "
				  (expand-file-name arxiv-default-download-folder)
				  nil nil pdfname))
    (if (directory-name-p newfile) (setq newfile (concat newfile pdfname)))
    (url-copy-file url newfile 1)
    (unless confirm
      (setq input (read-char-exclusive (format "%s saved. Open pdf? (y/N) " newfile)))
      (when (or (equal input ?y) (equal input ?Y))
	(funcall arxiv-pdf-open-function newfile)))
    ;; it is necessary to return a relative path in order to use the bib entry on diferents os,
    ;; in my case I use only the pdfname because helm-bibtex will manage the open function
    pdfname))

(defun freed034/arxiv-export-bibtex-to-string (&optional pdfpath)
  "Generate a bibtex entry according to the current arxiv entry.
Also add a relative link to PDFPATH in bibtex entry if it is specified.
It returns a string buffer containing the bibtex entry. This
function is a part of arXiv mode, and is supposed to be called by
`arxiv-export-bibtex' or `arxiv-export-bibtex-to-buffer'."
  (let*
      ((entry (nth arxiv-current-entry arxiv-entry-list))
       (title (cdr (assoc 'title entry)))
       (id (cdr (assoc 'id entry)))
       (author-list (cdr (assoc 'author entry)))
       (abstract (cdr (assoc 'abstract entry)))
       (year (cdr (assoc 'date entry)))
       (url (cdr (assoc 'url entry)))
       (journal (cdr (assoc 'journal entry)))
       (doi (cdr (assoc 'doi entry)))
       (author) (key) (bibtex-info))
    (setq author-list (mapcar (lambda (name) (progn
			(string-match "\\(.+\\) \\([^ ]+?\\)$" name)
			(setq name (concat (match-string 2 name) ", " (match-string 1 name)))))
			       author-list))
    (string-match "^[0-9]+" year)
    (setq year (match-string 0 year))
    (setq author (mapconcat #'identity author-list " and "))
    (setq abstract (replace-regexp-in-string "^ +" "" abstract))
    (setq abstract (replace-regexp-in-string " +$" "" abstract))
    (setq bibtex-info (format "@article{,
title = {%s},
author = {%s},
year = {%s}
}" title author year))
    (with-temp-buffer
      (insert bibtex-info)
      (bibtex-mode t)
      (bibtex-set-dialect 'BibTeX t)
      (setq key (bibtex-generate-autokey)))
    (setq bibtex-info (format "@article{%s,
title = {%s},
author = {%s},
abstract = {%s},
archivePrefix = {arXiv},
eprint = {%s},
url = {%s},
year = {%s}" key title author abstract id url year))
    (when doi
      (setq bibtex-info (concat bibtex-info (format ",\ndoi = {%s}" doi))))
    (when journal
      (setq bibtex-info (concat bibtex-info (format ",\njournal = {%s}" journal))))
    (when pdfpath
      (setq bibtex-info (concat bibtex-info (format ",\nfile = {:%s:PDF}" (concat "arXiv/" pdfpath)))))
    (setq bibtex-info (concat bibtex-info "\n}\n"))
    bibtex-info))

(use-package arxiv-mode
  :ensure t
  :bind (("\C-c \C-a r" . arxiv-read-recent)
         ("\C-c \C-a a" . arxiv-read-author)
         ("\C-c \C-a n" . arxiv-read-new)
         ("\C-c \C-a s" . arxiv-search))
  :config (progn
            (setq arxiv-default-download-folder (concat pdf-files-directory "/arXiv/")
                  arxiv-default-bibliography (concat bib-files-directory "/arXiv.bib")
                  ;; arxiv-startup-with-abstract-window t
                  arxiv-title-face "Noto Sans"
                  arxiv-abstract-face "Noto Sans"
                  arxiv-pdf-open-function (lambda (fpath) (call-process "zathura" nil 0 nil "-a" "evince" fpath)))

            (advice-add 'arxiv-download-pdf :override #'freed034/arxiv-download-pdf)
            (advice-add 'arxiv-export-bibtex-to-string :override #'freed034/arxiv-export-bibtex-to-string))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Google translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'google-translate)
;;(require 'google-translate-default-ui)
(require 'google-translate-smooth-ui)

;;(global-set-key "\C-ct" 'google-translate-at-point)
;;(global-set-key "\C-cT" 'google-translate-query-translate)
(global-set-key "\C-ct" 'google-translate-smooth-translate)

(setq google-translate-translation-directions-alist
      '(("en" . "es")("en" . "fr")))

(setq google-translate-backend-method 'curl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default web browser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (global-set-key "\C-cw" 'browse-url-at-point)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "luakit")

;; (setq browse-url-browser-function 'browse-url-firefox)
;; (setq browse-url-browser-function 'w3m-browse-url)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; W3m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'w3m-load nil t)
;; (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL" t)

(use-package w3m
  :demand t
  :bind (:map w3m-mode-map
              ("R" . freed034/w3m-toggle-readability
               ;; "\M-o" . ace-link-w3m)
              ))
  :custom
  (w3m-search-default-engine "duckduckgo")
  (w3m-quick-start nil)
  (w3m-display-mode 'plain)
  (w3m-use-title-buffer-name t)
  ;; (w3m-key-binding 'info)
  :config
  (defun freed034/w3m-toggle-readability (&arg)
    "Toggle readability and reload the current page"
    (interactive "P")
    (w3m-toggle-filtering nil)
    (w3m-realod-this-page)
    (w3m-toggle-filtering nil))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sage-shell-mode)

;; Run SageMath by M-x run-sage instead of M-x sage-shell:run-sage
(sage-shell:define-alias)

;; Turn on eldoc-mode in Sage terminal and in Sage source files
(add-hook 'sage-shell-mode-hook #'eldoc-mode)
(add-hook 'sage-shell:sage-mode-hook #'eldoc-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Slime
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Replace "sbcl" with the path to my implementation
(setq inferior-lisp-program "sbcl")
(load (expand-file-name "~/.quicklisp/slime-helper.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lean
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package lean4-mode
;;   :straight (lean4-mode
;;              :type git
;;              :host github
;;              :repo "leanprover/lean4-mode"
;;              :files ("*.el" "data"))
;;   ;; to defer loading the package until required
;;   :commands (lean4-mode)
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-hook 'after-init-hook 'global-company-mode)
;; (setq company-idle-delay 2)
;; (setq company-minimum-prefix-length 3)
;; (setq company-selection-wrap-around t)
;; (company-tng-configure-default)

;; ;; Trigger completion on Shift-Space
;; (global-set-key (kbd "S-SPC") #'company-complete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lua mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lua-mode
  :ensure t
  :commands (lua-mode)
  :mode ("\\.lua" . lua-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deft
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package deft
  :bind ("<f8>" . deft)
  :commands (deft)
  :config (setq deft-directory user-ORG
		deft-extensions '("org")
		deft-use-filename-as-title t
		deft-current-sort-method 'title)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Root
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun freed034/edit-current-file-as-root ()
  "Edit the file that is associated with the current buffer as root"
  (interactive)
  (if (buffer-file-name)
      (progn
        (setq file (concat "/sudo:root@localhost:" (buffer-file-name)))
        (find-file file))
    (message "Current buffer does not have an associated file.")))
