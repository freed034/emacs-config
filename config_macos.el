;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(require 'server) ;; package for the server
;;(unless (server-running-p) (server-start)) ;; start a server unless another is running

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(add-to-list 'default-frame-alist '(undecorated . t))

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

(setq default-frame-alist '((width . 160) (height . 60) (vertical-scroll-bars . nil) (font . "SF Mono-13")))
;; (add-to-list 'default-frame-alist '(font . "Monospace-13"))
;; (add-to-list 'default-frame-alist '(font . "Menlo-13"))
;; (add-to-list 'default-frame-alist '(font . "SF Mono-13"))

(repeat-mode 1)
;; don't interrupt me with native compilation warnings
(setq native-comp-async-report-warnings-errors nil)

(require 'iso-transl) ;; utilise the acents, included the circonflex accent for the math mode
(setq-default indent-tabs-mode t) ;; disable the use of tabs
;;(require 'fill-column-indicator) ;; load fill-column-indicator mode

(add-hook 'text-mode-hook 'turn-on-visual-line-mode) ;; add the line wrap for all text modes

(fset 'yes-or-no-p 'y-or-n-p) ;; y/n instead of yes/no
(global-auto-revert-mode t) ;; auto revert file on change
(setq vc-follow-symlinks t) ;; set by default the option yes for follow links

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacsmac-app
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq mac-command-modifier nil)
(setq mac-mouse-wheel-mode t)
(setq mac-option-modifier (quote (:ordinary meta)))
(setq mac-right-option-modifier nil)

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

;; (require 'winum)
;; (winum-mode)

(use-package winum
  :ensure t
  :init (winum-mode t)
  ;; :config (winum-set-keymap-prefix (kbd "\C-c")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exec-path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(setenv "PKG_CONFIG_PATH" "/usr/local/Cellar/gmime/3.2.3/lib/pkgconfig/:/usr/local/Cellar/libffi/3.2.1/lib/pkgconfig/")
;;(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin")) ;; add texbin to the PATH for emacs
;;(setq exec-path (append exec-path '(":/Library/TeX/texbin"))) ;; add texbin to the exec-path for emacs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (E)Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'lisp-mode-hook 'display-line-numbers-mode)
(add-hook 'emacs-lisp-mode-hook 'display-line-numbers-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linum (line numbers)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (global-linum-mode t) ;; enable line numbers globally
;; ;;(setq linum-format "%d ") ;; numbers begin in the left
;; ;;(setq linum-format "%4d \u2502")
;; (setq linum-format "%4d ")
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
;; 				     org-mode
;;                                   eshell-mode
;;                                   elfeed-search-mode
;;                                   elfeed-show-mode
;;                                   treemacs-mode
;;                                   arxiv-mode
;;                                   arxiv-abstract-mode))

;; (defun linum-on ()
;;   (unless (or (minibufferp)
;;               (member major-mode linum-disabled-modes-list)
;;               (> (buffer-size) 3000000)) ;; disable linum on buffer greater than 3MB, otherwise it's unbearably slow
;;     (linum-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sublimity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(sublimity-mode 1)
;;(require 'sublimity)
;;(require 'sublimity-scroll)
;;;;(require 'sublimity-map) ;; experimental
;;;;(require 'sublimity-attractive)

;;(setq sublimity-scroll-weight 10
;;      sublimity-scroll-drift-length 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Left-ALT-as-Meta, Right-Alt-for-Special-Characters (for Emacs.app)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(setq ns-alternate--modifier 'meta)
;;(setq ns-right-alternate-modifier nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Function to load my org file TODOs_personal
;; (defun freed034/load-personal ()
;;   (interactive)
;;   (find-file "org/pers.org"))

;; ;; Function to load my org file TODOs_work
;; (defun freed034/load-work ()
;;   (interactive)
;;   (find-file "org/work.org"))

;; Function to desactive linum mode when i am using org-mode
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
;; (global-set-key (kbd "\C-c w") 'freed034/load-work)
;; (global-set-key (kbd "\C-c p") 'freed034/load-personal)

;; Shortcut for undo
(global-set-key (kbd "\C-z") 'undo)

;; Shortcut for compile in TeX
(add-hook 'LaTeX-mode-hook #'(lambda () (define-key LaTeX-mode-map (kbd "\C-c \C-a") 'TeX-command-run-all)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helm
  :init (setq helm-command-prefix-key "\C-c h")
  :bind (("\C-x f" . helm-find)
	 ("\C-x \C-f" . helm-find-files)
	 ("\C-x \C-r" . helm-recentf)
	 ("\C-x b" . helm-mini)
	 ("\C-x \C-b" . buffer-menu-other-window)
	 ("\M-x" . helm-M-x)
         ("\M-y" . helm-show-kill-ring)
         ("\M-/" . helm-dabbrev))
  :config (progn
	    (setq helm-autoresize-max-height 0
		  helm-autoresize-min-height 20
		  helm-echo-input-in-header-line t
		  helm-scroll-amount 4 ;; scroll 4 lines other window using M-<next>/M-<prior>
		  helm-quick-update t ;; do not display invisible candidates
		  helm-idle-delay 0.01 ;; be idle for this many seconds, before updating in delayed sources.
		  helm-input-idle-delay 0.01 ;; be idle for this many seconds, before updating candidate buffer
		  helm-split-window-default-side 'other ;; open helm buffer in another window
		  helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
		  helm-candidate-number-limit 200 ;; limit the number of displayed canidates
		  helm-move-to-line-cycle-in-source nil ;; move to end or beginning of source when reaching top or bottom of source.
		  ;; helm-command
		  helm-M-x-requires-pattern 0 ;; show all candidates when set to 0
		  )
	    (bind-keys :map helm-command-map
		       ("a" . helm-ag)
		       ("o" . helm-occur)
		       ("y" . yas-insert-snippet))
	    ;; (bind-keys ("\C-x b" . helm-mini)
	    ;; 	       ("\C-x \C-b" . buffer-menu-other-window)
	    ;; 	       ("\M-x"   . helm-M-x)
	    ;; 	       ("\M-y"   . helm-show-kill-ring))
	    (bind-keys :map helm-map
		       ("\C-o" . nil)
		       ("\M-o" . helm-previous-source)
		       ("TAB" . helm-execute-persistent-action)
		       ("\C-i" . helm-execute-persistent-action)
		       ("\C-z" . helm-select-action))
	    ;; (bind-keys :map helm-find-files-map
	    ;; 	       ("C-h" . delete-backward-char)
	    ;; 	       ("C-i" . helm-execute-persistent-action))

	    (helm-autoresize-mode 1)
	    (helm-adaptive-mode 1)

	    (setq helm-ff-search-library-in-sexp t ;; search for library in `require' and `declare-function' sexp.
		  helm-ff-file-name-history-use-recentf t
		  ;; list of the buffers that I don't want to see
		  helm-boring-buffer-regexp-list (list (rx "*magit-")
						       (rx ".git")
						       (rx "*helm")
						       (rx "*Mini")
						       (rx "*Echo")
						       (rx "*mu4e")
						       ;;(rx "*scratch")
						       (rx "*Completions")
						       (rx "*http melpa.org:80*")
						       (rx "*temp*")
						       (rx "*autoload*")
						       (rx "*code-conversion-work*")
						       (rx "*code-converting-work*")
						       (rx "*tip*"))
		  ;; do not show these files in helm buffer
		  helm-boring-file-regexp-list '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$" "\\.DS_Store"))

	    (helm-mode t))
  )

(use-package helm-ag
  :ensure t
  ;; :defer t
  )

;; (use-package helm-swoop
;;   :bind (("M-s o" . helm-swoop)
;; 	    ("M-O"   . helm-swoop-back-to-last-point)
;; 	    ("M-s /" . helm-multi-swoop)
;; 	    ;;("\C-c \M-O" . helm-multi-swoop-all))
;;   :config (progn
;; 	    ;; Save buffer when helm-multi-swoop-edit complete
;; 	    (setq helm-multi-swoop-edit-save t)
;; 	    ;; If this value is t, split window inside the current window
;; 	    (setq helm-swoop-split-with-multiple-windows nil)
;; 	    ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
;; 	    (setq helm-swoop-split-direction 'split-window-horizontally)
;; 	    ;; If nil, you can slightly boost invoke speed in exchange for text color
;; 	    (setq helm-swoop-speed-or-color t)
;; 	    (bind-keys :map isearch-mode-map
;; 		       ("M-o" . helm-swoop-from-isearch))
;; 	    (bind-keys :map helm-swoop-map
;; 		       ;; ("\M-i" . helm-swoop-from-evil-search)
;; 		       ("M-o" . helm-multi-swoop-all-from-helm-swoop)))
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ido mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (setq ido-ignore-extensions t) ;; completation-ignored-extensions
;; (ido-mode t)

;; (add-to-list 'ido-ignore-files "\\.DS_Store") ;; Ignore .DS_Store files with ido mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Handling file properties for ‘CREATED’ & ‘LAST_MODIFIED’
;; Reference init file in https://github.com/zaeph

(defun freed034/org-find-time-file-property (property &optional anywhere)
  "Return the position of the time file PROPERTY if it exists. When ANYWHERE is non-nil, search beyond the preamble."
  (save-excursion (goto-char (point-min))
		  (let ((first-heading
			 (save-excursion
			   (re-search-forward org-outline-regexp-bol nil t))))
		    (when (re-search-forward (format "^#\\+%s:" property)
					     (if anywhere nil first-heading)
					       t)
		      (point)))))

(defun freed034/org-has-time-file-property-p (property &optional anywhere)
  "Return the position of time file PROPERTY if it is defined. As a special case, return -1 if the time file PROPERTY exists but
is not defined."
  (when-let ((pos (freed034/org-find-time-file-property property anywhere)))
    (save-excursion
      (goto-char pos)
      (if (and (looking-at-p " ")
               (progn (forward-char)
                      (org-at-timestamp-p 'lax)))
          pos
        -1))))

(defun freed034/org-set-time-file-property (property &optional anywhere pos)
  "Set the time file PROPERTY in the preamble. When ANYWHERE is non-nil, search beyond the preamble.
If the position of the file PROPERTY has already been computed, it can be passed in POS."
  (when-let ((pos (or pos
                      (freed034/org-find-time-file-property property))))
    (save-excursion
      (goto-char pos)
      (if (looking-at-p " ")
          (forward-char)
        (insert " "))
      (delete-region (point) (line-end-position))
      (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
        (insert now)))))

(defun freed034/org-set-last-modified ()
  "Update the LAST_MODIFIED file property in the preamble."
  (when (derived-mode-p 'org-mode)
    (freed034/org-set-time-file-property "LASTMOD")))

  ;; ;; Set faces for heading levels
  ;; (dolist (face '((org-level-1 . 1.2)
  ;;                 (org-level-2 . 1.1)
  ;;                 (org-level-3 . 1.05)
  ;;                 (org-level-4 . 1.0)
  ;;                 (org-level-5 . 1.1)
  ;;                 (org-level-6 . 1.1)
  ;;                 (org-level-7 . 1.1)
  ;;                 (org-level-8 . 1.1)))
  ;; (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

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

(defun freed034/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(defun freed034/org-mode-setup ()
  (org-indent-mode)
  ;; (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  ;; :pin org
  :commands (org-capture org-agenda)
  :hook ((org-mode . freed034/org-mode-setup)
	 (before-save . freed034/org-set-last-modified))
  :config (progn
	    (setq org-directory user-ORG)
	    (setq org-ellipsis " ▾")
	    (setq org-log-done 'time)
	    ;;(setq org-log-done 'note)
	    (setq org-agenda-window-setup (quote current-window)) ;; Open agenda in current window
	    (define-key global-map (kbd "\C-c a") 'org-agenda) ;; Set key for agenda
	    ;; (setq org-list-description-max-indent 5) ;; Set maximum indentation for description lists
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

	    (advice-add 'org-refile :after 'org-save-all-org-buffers) ;; Save Org buffers after refiling!

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
	    ;;(define-key global-map (kbd "C-c c") 'org-capture)
	    (define-key global-map (kbd "\C-c j") (lambda () (interactive) (org-capture nil "jj")))

	    ;;(define-key global-map (kbd "\C-cl") 'org-store-link)
	    ;;(define-key global-map (kbd "\C-cb") 'org-ido)
	    
	    (freed034/org-font-setup))
  )

(defun freed034/org-mode-visual-fill ()
  (setq visual-fill-column-width 200)
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
  :custom ((org-roam-directory (file-truename user-ROAM))
	   (org-roam-completion-everywhere t))
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
  :bind (("\C-c z" . orb-insert-link)
	 (:map org-mode-map ("\C-c n a" . orb-note-actions)))
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config (progn
	    (require 'org-ref)
	    (setq orb-insert-interface 'helm-bibtex
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
			       (shell . t)
			       (latex .t)
			       ;; Add other languages here
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

;; Use ipython as the default python interpreter
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

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

;; (projectile-mode 1)
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
;; ;; (projectile-global-mode)
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
;;          ("\C-c c"   . org-capture))
;;   :config (progn
;; 	    (setq org-projectile-projects-file (concat user-RES "TODOs.org"))
;; 	    ;;(setq org-projectile-capture-template (format ))
;; 	    (add-to-list 'org-capture-templates
;; 			 (org-projectile-project-todo-entry
;; 			  :capture-template "* TODO %^{Todo} %^g\n:PROPERTIES:\n:Created: %T\n:File: %f\n:Link: %a\n:About: %?:END:\n"
;; 			  :capture-character "t2"
;; 			  :capture-heading "Linked Project TODO"))
;; 	    ;; (add-to-list 'org-capture-templates
;; 	    ;;              (org-projectile-project-todo-entry
;; 	    ;;               :capture-template "* IDEA %^{Idea} %^g\n:PROPERTIES:\n:Created: %T\n:File: %f\n:Link: %a\n:About: %?:END:\n"
;; 	    ;;               :capture-character "i2"))
;; 	    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files))))
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Treemacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package treemacs
  :ensure t
  :init (with-eval-after-load 'winum (define-key winum-keymap (kbd "\M->") #'treemacs-select-window))
  :bind (:map global-map
              ("\C-x t t"   . treemacs)
              ("\C-x t s"   . treemacs-select-window)
              ("\C-x t 1"   . treemacs-delete-other-windows)
              ("\C-x t p"   . treemacs-projectile)
	      ;;("\C-x t \C-t" . treemacs-find-file)
	      ;;("\C-x t \M-t" . treemacs-find-tag)
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
		  treemacs-indentation-string         " "
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
		  treemacs-sorting                    'alphabetic-asc ;; desc
		  treemacs-space-between-root-nodes   nil
		  treemacs-tag-follow-cleanup         t
		  treemacs-tag-follow-delay           1.5
		  ;; treemacs-resize-icons               44 ;; the default width and height of icons is 22 pixels. For Hi-DPI display, 44.
		  treemacs-width                      26))

  ;;(pcase (cons (not (null (executable-find "git"))) (not (null (executable-find "python3"))))
  ;;          (`(t . t) (treemacs-git-mode 'extended))
  ;;          (`(t . _) (treemacs-git-mode 'simple)))
  )

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile)
  )

(use-package treemacs-magit
  :ensure t
  :after (treemacs magit)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Openwith
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package openwith
;;   :init (openwith-mode 1)
;;   :config (setq openwith-associations '(("\\.pdf\\'" "/Applications/Skim.app" (file))))
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'auto-complete)
;;(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

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

;;(defun flymake-get-tex-args (file-name)
;;(list "pdflatex"
;;(list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spell-checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (dolist (hook '(text-mode-hook)) (add-hook hook #'(lambda () (flyspell-mode 1))))

;; ;; To disable flyspell for change-log-mode and log-edit-mode
;; (dolist (hook '(change-log-mode-hook log-edit-mode-hook)) (add-hook hook (lambda () (flyspell-mode -1))))

(global-set-key "\C-c i" 'ispell-buffer)
(setq flyspell-issue-message-flag nil)
(setq ispell-dictionary "english")
(setq ispell-program-name "aspell")
(setq ispell-silently-savep t)
(setq ispell-list-command "--list")
(setq ispell-extra-args '("--sug-mode=ultra" "--mode=tex"))
(setq ispell-personal-dictionary (concat user-DIR "data/perso_en_dict"))

;; (defun freed034/my-flyspell-en ()
;;   (interactive)
;;   (ispell-change-dictionary "english")
;;   (setq ispell-extra-args '("--sug-mode=ultra" "--mode=tex" "--reverse" "--lang=en_US"))
;;   (setq ispell-personal-dictionary (concat user-DIR "data/perso_en_dict")) ;; personal english dictionary
;;   (flyspell-buffer))

;; (defun freed034/my-flyspell-fr ()
;;   (interactive)
;;   (ispell-change-dictionary "french")
;;   (setq ispell-extra-args '("--sug-mode=ultra" "--mode=tex" "--reverse" "--lang=fr_FR"))
;;   (setq ispell-personal-dictionary (concat user-DIR "data/perso_fr_dict")) ;; personal french dictionary
;;   (flyspell-buffer))

;; (defun freed034/my-flyspell-sp ()
;;   (interactive)
;;   (ispell-change-dictionary "spanish")
;;   (setq ispell-extra-args '("--sug-mode=ultra" "--mode=tex" "--reverse" "--lang=es_ES"))
;;   (setq ispell-personal-dictionary (concat user-DIR "data/perso_sp_dict")) ;; personal spanish dictionary
;;   (flyspell-buffer))

;; (require 'easymenu)
;;     (easy-menu-add-item nil '("tools" "spell")
;; 			["Select Default Dict" my-flyspell-en t]
;; 			["Select French Dict" my-flyspell-fr t]
;; 			["Select Spanish Dict" my-flyspell-sp t])

;; (add-hook 'LaTeX-mode-hook #'(lambda ()
;; 			      (setq ispell-tex-skip-alists
;; 				    (list
;; 				     (append
;; 				      (car ispell-tex-skip-alists)
;; 				      '(("[^\\]\\$" . "[^\\]\\$")))
;; 				     (cadr ispell-tex-skip-alists)))))

;; (add-hook 'LaTeX-mode-hook #'(lambda ()
;; 			     (setcar ispell-tex-skip-alists
;; 				     (append
;; 				      (car ispell-tex-skip-alists)
;; 				      '(("[^\\]\\$" . "[^\\]\\$"))))))

;; (eval-after-load "tex-ispell"
;;   ’(progn
;;      ;; (TeX-ispell-skip-setcar
;;      ;;  ’(("\\\\mymacro" ispell-tex-arg-end)))
;;      (TeX-ispell-skip-setcdr
;;       '(("[^\\]\\$" . "[^\\]\\$")))))

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
  (outline-minor-mode 1))

(add-hook 'LaTeX-mode-hook 'freed034/turn-on-outline-minor-mode)
(add-hook 'latex-mode-hook 'freed034/turn-on-outline-minor-mode)
(setq outline-minor-mode-prefix "\C-c \C-o") ; Or something else

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AucTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Only start server for pdf viewer when in latex mode
;;(add-hook 'LaTeX-mode-hook 'server-start) ;; this line is used with emacs server
;;(setq TeX-PDF-mode t) ;; this line is equivalent to the next one
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

;; Standard emacs/latex config
(add-hook 'LaTeX-mode-hook (lambda ()
                             (setq TeX-auto-save t)
                             (setq TeX-parse-self t)
                             (setq TeX-save-query nil) ;; autosave before compiling
                             (setq-default TeX-master nil)))

;; automatic detection of master file form the current open tex files
(defun freed034/guess-TeX-master (filename)
  (let ((candidate nil) (filename (file-name-nondirectory filename)))
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

;; automatic detection of local variables in tex files
(defun freed034/need-new-auctex-file-variables (filename)
  "Return nil if there are not local variable and t otherwise."
  (let (answer nil))
  
  answer)

;; automatic add some local variables to a tex file
(defun freed034/add-new-auctex-file-variables ()
  (interactive)
  (if (and (not buffer-read-only)
           (string= (file-name-extension (buffer-file-name)) "tex")
	   (freed034/need-new-auctex-file-variables (buffer-file-name)))
      (progn
	(add-file-local-variable 'coding 'utf-8-unix)
	;; (add-file-local-variable 'eval '(flyspell-mode 1))
	(add-file-local-variable 'ispell-local-dictionary '"english")
	;; (add-file-local-variable 'TeX-engine 'luatex)
	(goto-char (point-min)))))

;; (add-hook 'LaTeX-mode-hook #'(lambda () (setq TeX-master (freed034/guess-TeX-master (buffer-file-name)))))
;; (add-hook 'LaTeX-mode-hook 'freed034/add-new-auctex-file-variables)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'display-line-numbers-mode)
;;(add-hook 'LaTeX-mode-hook 'auto-fill-mode) ;; enable auto-fill mode, nice for text

(setq TeX-electric-sub-and-superscript t)
;;(setq LaTeX-electric-left-right-brace t)

;;;; Brackets for the math mode
;; (add-hook ’plain-TeX-mode-hook #'(lambda () (set (make-variable-buffer-local ’TeX-electric-math)
;;                                                (cons "$" "$"))))
;; (add-hook ’LaTeX-mode-hook #'(lambda () (set (make-variable-buffer-local ’TeX-electric-math)
;;                                            (cons "\\(" "\\)"))))
;; (add-hook 'LaTeX-mode-hook #'(lambda () (set (make-local-variable 'TeX-electric-math)
;; 			                   (cons "\\(" "\\)"))))

;; Enable synctex correlation
(add-hook 'LaTeX-mode-hook #'(lambda () (setq TeX-source-correlate-mode t)
			                (setq TeX-source-correlation-method 'synctex)
			                (setq TeX-source-correlate-start-server t)))

;; Enable synctex generation. Even though the command shows
;; as "latex" pdflatex is actually called
'(LaTeX-command "latex -synctex=1")
	
;; Set ispell dictionary using an auctex

(add-hook 'TeX-language-en-hook #'(lambda () (ispell-change-dictionary "english")))

(add-hook 'TeX-language-fr-hook #'(lambda () (ispell-change-dictionary "french")))

(add-hook 'TeX-language-fr-hook #'(lambda () (ispell-change-dictionary "spanish")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-hook 'LaTeX-mode-hook #'(lambda () (push
;;                              '("Latexmk" "latexmk -pdf %s" TeX-run-TeX nil t ;;"latexmk -pdf -pvc %t" TeX-run-TeX nil t
;;                                :help "Run Latexmk on file")
;;                              TeX-command-list)))

;; (add-hook 'TeX-mode-hook #'(lambda () (setq TeX-command-default "Latexmk")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Xelatex and nomenclature compilation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load "tex" '(add-to-list 'TeX-command-list
                                     '("XeLaTeX" "xelatex -interaction=nonstopmode %s"
                                       TeX-run-command t t :help "Run xelatex") t))

(eval-after-load "tex" '(add-to-list 'TeX-command-list 
                                     '("Nomenclature" "makeindex %s.nlo -s nomencl.ist -o %s.nls"
                                       (lambda (name command file)
					 (TeX-run-compile name command file)
                                         (TeX-process-set-variable file 'TeX-command-next TeX-command-default))
                                       nil t :help "Create nomenclature file")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Skim as default pdf viewer, enable source <-> PDF sync
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background

(setq TeX-view-program-list '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b")))
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pdf-tools as default pdf viewer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-tools-install)
  ;; :bind ("\C-c \C-g" . pdf-sync-forward-search)
  :config (progn
	    (setq-default pdf-view-display-size 'fit-page)
	    (define-key pdf-view-mode-map (kbd "\C-s") 'isearch-forward)
	    (setq mouse-wheel-follow-mouse t)
	    (setq pdf-view-resize-factor 1.10))
  )

;; ;; Use pdfview with auctex
;; (add-hook 'LaTeX-mode-hook 'pdf-tools-install)

;; ;; Use pdf-tools to open PDF files
;; (setq TeX-view-program-list '(("PDF Tools" "TeX-pdf-tools-sync-view")))
;; (setq TeX-view-program-selection '((output-pdf "PDF Tools")))

;; ;; Update PDF buffers after successful LaTeX runs
;; (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

;; ;; ;; Pdf-tools in another frame
;; ;; (defun freed034/framesMenus-display-buffer-use-some-frame (fun &rest args)
;; ;;   "Use `display-buffer-use-some-frame' as `display-buffer-overriding-action'. Then run FUN with ARGS."
;; ;;   (let ((display-buffer-overriding-action '(display-buffer-use-some-frame)))
;; ;;     (apply fun args)))

;; ;; (advice-add 'TeX-pdf-tools-sync-view :around #'freed034/framesMenus-display-buffer-use-some-frame)
;; ;; (advice-add 'pdf-sync-backward-search-mouse :around #'freed034/framesMenus-display-buffer-use-some-frame)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reftex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(setq LaTeX-eqnarray-label "eq"
      LaTeX-equation-label "eq"
      LaTeX-figure-label "fig"
      LaTeX-table-label "tab"
      LaTeX-myChapter-label "chap")

;; TeX-newline-function 'reindent-then-newline-and-indent

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
		  bibtex-completion-pdf-symbol "⌘"
		  bibtex-completion-notes-symbol "✎"
		  bibtex-completion-pdf-field "file"
		  bibtex-completion-pdf-open-function (lambda (fpath) (call-process "open" nil 0 nil "-a" "/Applications/Skim.app" fpath))
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
									  "* Goals")
                  bibtex-completion-display-formats '((t . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:25} ${title:*}")))

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
  :defer t
  :after (:any org pdf-tools)
  :config (setq org-noter-notes-window-location 'other-frame
		org-noter-always-create-frame nil
		org-noter-hide-other nil
		org-noter-notes-search-path (list org-files-directory))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "\C-x d") (lambda () (interactive) (dired (getenv "HOME"))))
(when (string= system-type "darwin") (setq dired-use-ls-dired nil))
(add-hook 'dired-load-hook #'(lambda () (require 'dired-x))) ;; load Dired X
(setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*$")
(setq dired-omit-mode t) ;; turn on omit mode.

;; Open the current file in an external application

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

;; Bind the minor mode command in dired for dired-git-info
(with-eval-after-load 'dired (define-key dired-mode-map ")" 'dired-git-info-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elfeed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file (concat user-DIR "data/elfeedrc.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bookmarks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq bookmark-default-file (concat user-DIR "data/bookmarks")
;;       bookmark-save-flag 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Shell pop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package shell-pop
  :ensure t
  :bind ("\C-c x" . shell-pop)
  :config (progn
	    (setq shell-pop-shell-type '("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell))))
	    (setq shell-pop-term-shell "/bin/bash")
	    ;; need to do this manually or not picked up by `shell-pop'
	    (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))
  )

;; (require 'shell-pop)
;; (shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
;; (shell-pop-term-shell "/bin/bash")
;; (shell-pop-universal-key "\C-c x")
;; (shell-pop-window-size 30)
;; (shell-pop-set-window-height 60) ;the number for the percentage of the selected window. if 100, shell-pop use the whole of selected window, not spliting.
;; (shell-pop-full-span t)
;; (shell-pop-set-window-position "bottom") ;shell-pop-up position. You can choose "top" or "bottom".

;; (shell-pop-set-internal-mode "ansi-term")
;; (shell-pop-set-internal-mode-shell "/bin/bash")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight-focus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package highlight-focus
;;   :load-path (concat user-DIR "packages/highlight-focus/")
;;   :config (setq highlight-focus:face 'mode-line
;; 		highlight-focus:face-property :background
;; 		highlight-focus:face-property-value "darkred")) ;; "darkblue"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dim other buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'after-init-hook #'(lambda ()
			       (when (fboundp 'auto-dim-other-buffers-mode)
				 (auto-dim-other-buffers-mode t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Awesome-tab
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(require 'awesome-tab)
;;(awesome-tab-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dimmer (visual highlight focus)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dimmer
  :ensure t
  :config (progn
	    (dimmer-configure-helm)
	    (dimmer-mode)
	    ;; from 0 (nothing) to 1 (total)
	    (setq dimmer-fraction 0.3))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Doom-modeline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1)
;;   :config (progn
;;             (setq doom-modeline-project-detection 'project)
;;             ;;(setq doom-modeline-icon (display-graphic-p))
;;             (setq doom-modeline-height 25)
;;             ;;(set-face-attribute 'mode-line nil :family "Noto Sans" :height 100)
;;             ;;(set-face-attribute 'mode-line-inactive nil :family "Noto Sans" :height 100)
;;             (setq doom-modeline-bar-width 2))
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Doom-themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'doom-themes)

;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
;;(load-theme 'doom-one t)
;;(load-theme 'doom-nord t)
;;(load-theme 'doom-opera t)
;;(load-theme 'doom-city-lights t)
(load-theme 'doom-dracula t)
;;(load-theme 'doom-molokai t)
;;(load-theme 'doom-vibrant t)
;;(load-theme 'doom-acario-dark t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Enable custom neotree theme (all-the-icons must be installed!)
;;(doom-themes-neotree-config)
;; or for treemacs users
(setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
(doom-themes-treemacs-config)

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

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
;; Packages themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(use-package nyx-theme) ;; load nyx theme
;;(load-theme 'spacemacs-dark t) ;; load spacemacs theme in a dark version
;;(load-theme 'nimbus t) ;; load nimbus theme
;;(load-theme 'exotica t) ;; load exotica theme
;;(load-theme 'dracula t) ;; load dracula theme
;;(load-theme 'molokai) ;; load color theme molokai
;;(load-theme 'cobalt t) ;; load cobalt theme
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
      (bibtex-mode)
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
      (setq bibtex-info (concat bibtex-info (format ",\nfile = {:%s}" (concat "arXiv/" pdfpath)))))
    (setq bibtex-info (concat bibtex-info "\n}\n"))
    bibtex-info))

(use-package arxiv-mode
  :ensure t
  :bind (("\C-x \C-a r" . arxiv-read-recent)
         ("\C-x \C-a a" . arxiv-read-author)
         ("\C-x \C-a n" . arxiv-read-new)
         ("\C-x \C-a s" . arxiv-search))
  :config (progn
            (setq arxiv-default-download-folder (concat pdf-files-directory "/arXiv/")
                  arxiv-default-bibliography (concat bib-files-directory "/arXiv.bib")
                  ;; arxiv-startup-with-abstract-window t
                  ;; arxiv-title-face "Noto Sans"
                  ;; arxiv-abstract-face "Noto Sans"
		  arxiv-pdf-open-function (lambda (fpath) (call-process "open" nil 0 nil "-a" "/Applications/Preview.app" fpath)))
	    
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

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "open")

;; (setq browse-url-browser-function 'browse-url-firefox)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Langtool
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package langtool
;;   :ensure t
;;   :commands (langtool-check-buffer langtool-check-buffer@fix-narrowing)
;;   :when (and langtool-installation-dir
;;              (file-exists-p langtool-installation-dir))
;;   :preface
;;   (defcustom langtool-installation-dir nil
;;     "Path to the local installation of langtool."
;;     :type '(choice (const :tag "not installed" nil)
;;                    string)
;;     :tag "Langtool installation directory"
;;     :group 'langtool)
;;   (defvar langtool-args
;;     (let ((ngrams (expand-file-name "ngrams-en-20150817"
;;                                     langtool-installation-dir)))
;;       (when-let (file-exists-p ngrams)
;;         (list (concat "--languageModel " ngrams)))))
;;   :custom
;;   (langtool-language-tool-jar
;;    (expand-file-name "languagetool-commandline.jar"
;;                      langtool-installation-dir))
;;   (langtool-java-user-arguments langtool-args)
;;   (langtool-language-tool-server-jar
;;    (expand-file-name "languagetool-server.jar"
;;                      langtool-installation-dir))
;;   (langtool-http-server-host "localhost")
;;   (langtool-server-user-arguments langtool-args)
;;   :config
;;   (define-advice langtool-check-buffer (:around (fn &optional lang) fix-narrowing)
;;     (save-mark-and-excursion
;;       (unless (use-region-p)
;;         (push-mark)
;;         (push-mark (point-max) nil t)
;;         (goto-char (point-min)))
;;       (funcall fn lang))))
