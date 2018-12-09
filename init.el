;;***********************************
;; PACKAGE INIT
;;***********************************
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
					;(setq package-enable-at-startup nil)

;;***********************************
;; STARTING MODES
;;***********************************
(exec-path-from-shell-initialize)
(desktop-save-mode 1)

(defun my-desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))
(add-hook 'auto-save-hook 'my-desktop-save)


;;***********************************
;;KEYBINDINGS
;;***********************************
(global-set-key [f5] 'httpd-start)
(global-set-key [f6] 'impatient-mode)
;;(global-set-key [f7] (lambda () (interactive) (browse-url "http://localhost:8080/imp/")))
;;(global-set-key [f8] (lambda () (interactive) (browse-url "http://localhost:8888/")))
;;(global-set-key [f9] (lambda () (interactive) (shell-command "open -a MAMP")))
;;(global-set-key (kbd "C-' ") 'ace-window)

;;***********************************
;; HIGHLIGHT INDENT GUIDES 
;;***********************************
(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-character ?\|)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)


;; Allow paste in terminal
(eval-after-load "term"
  '(define-key term-raw-map (kbd "C-c C-y") 'term-paste))


;;***********************************
;; AUTO-COMPLETE 
;;***********************************

(require 'auto-complete)
(auto-complete-mode 1)
;;Default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(require 'yasnippet)
(yas-global-mode 1)

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(defvar flymake-additional-compilation-flags nil)
(put 'flymake-additional-compilation-flags 'safe-local-variable 'listp)

;;semantic mode
(semantic-mode 1)
;; le'ts define a fx which adds semantic as a suggestion backend
;; and hook this fx to c-modec-mmon hook

(defun my:add-semantic-to-autocomplete()
  (add-to-list 'ac-sources 'ac-source-semantic)
  )
(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)
;; turn on ede mode
(global-ede-mode 1)
;;create a project for our program



;; Indenting hotkeys
(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(global-unset-key (kbd "C-k"))
(global-set-key (kbd "C-k C-d") 'indent-buffer)
(global-set-key (kbd "C-k C-c") 'comment-region)
(global-set-key (kbd "C-k C-u") 'uncomment-region)




;;***********************************
;; C-AC
;;***********************************
(defun my:acc-c-header-init()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (setq achead:include-directories
	(append '("/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/9.1.0/include"
		  "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include"
		  "/usr/local/include")
		achead:include-directories)))
;;(add-hook 'c-mode-hook 'my:ac-c-header-init)

(defun my:flymake-google-init-c()
  (require 'flymake-google-cpplint)
  (custom-set-variables
   '(flymake-google-cpplint-command "/Library/Frameworks/Python.framework/Versions/3.6/bin/cpplint"))
  (flymake-google-cpplint-load)
  )
(add-hook 'c++-mode-hook 'my:flymake-google-init-c)

;;gogole-c-style with emacs
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;;***********************************
;; C++-AC
;;***********************************
(defun my:acc_c++-header-init()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (setq achead:include-directories
	(append '((my:acc-c-header-init)
		  "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1")
		achead:include-directories)))
;;;https://emacs.stackexchange.com/questions/22556/auto-complete-c-headers-just-works-with-c-no-result-with-c
(add-hook 'c++-mode-hook 'my:ac-c++-header-init)


;; fly-make-google-cpplint-load
(defun my:flymake-google-init()
  (require 'flymake-google-cpplint)
  (custom-set-variables
   '(flymake-google-cpplint-command "/Library/Frameworks/Python.framework/Versions/3.6/bin/cpplint"))
  (flymake-google-cpplint-load)
  )
(add-hook 'c++-mode-hook 'my:flymake-google-init)

(show-paren-mode 1)
(dumb-jump-mode 1)

(add-to-list 'load-path "~./emacs.d/plugins")
(put 'set-goal-column 'disabled nil)




;;***********************************
;; EVIL MODE 
;;***********************************
(global-set-key (kbd "C-c C-e") 'evil-mode)
(evil-mode 1)

;;***********************************
;; iedit
;;***********************************
(define-key global-map (kbd "C-c C-;") 'iedit-mode)

;;***********************************
;; NEOTREE
;;***********************************
;(neotree)
(neotree-dir "~/Documents/")
(global-set-key (kbd "C-=") 'neotree-toggle)
;(neotree-hidden-file-toggle)

;;***********************************
;; HASKELL
;;***********************************
(require 'haskell-mode)
;;(require 'intero)
;;(add-hook 'haskell-mode-hook 'intero-mode)
(require 'flycheck)
(add-hook 'haskell-mode-hook #'flycheck-haskell-setup)
;;(setq flycheck-check-syntax-automatically '(save new-line))
;;(flycheck-add-next-checker 'intero '(warning . haskell-hlint))

;;***********************************
;; BACKUP DIRECTORY
;;***********************************
(setq backup-directory-alist `(("." . "~/.emacs.d/backup/")))
					;(setq backup-by-copying)

;;***********************************
;;HTTPD SERVER and Skewer Mode Init
;;***********************************
(require 'simple-httpd)
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)


;;***********************************
;; MC-CURSORS
;;***********************************
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;***********************************
;; ETC
;;***********************************
(global-set-key (kbd "C-z") 'buffer-menu)

(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<down>") 'shrink-window)
(global-set-key (kbd "M-<up>") 'enlarge-window)

;;***********************************
;; LaTeX
;;***********************************
(defun LaTexKey ()
  (lambda ()
    (local-set-key (kbd "C-c C-'") #'TeX-next-error)
    (local-set-key (kbd "C-c <escape>") #'TeX-previous-error)))

(add-hook 'LaTeX-mode-hook
          (LaTexKey))

;;***********************************
;; Scheme Mode
;;***********************************
;;; Always do syntax highlighting
(global-font-lock-mode 1)
(add-hook 'geiser-mode (lambda () (geiser-set-scheme 'mit)))

;;; Also highlight parens
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)

(add-to-list 'auto-mode-alist '("\\.scm\\'" . racket-mode))

;;; This is the binary name of my scheme implementation
(setq scheme-program-name "mit")

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


;;***********************************
;; SLIME  for SBCL
;;***********************************
					;(add-to-list 'load-path "/the/path/to/slime")
(require 'slime-autoloads)
(require 'slime)
;;(setq inferior-lisp-program "/opt/sbcl/bin/sbcl")

(add-hook 'lisp-mode-hook 'slime-mode)
(setq inferior-lisp-program "sbcl" ; Steel Bank Common Lisp
      slime-contribs '(slime-fancy))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
					;(setq inferior-lisp-program "clisp") 
					;(add-hook 'lisp-mode-hook 'slime-mode)
;; Optionally, specify the lisp program you are using. Default is "lisp"


;;***********************************
;; CEDIT
;;***********************************
;; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
;; IMPORTANT: For Emacs >= 23.2, you must place this *before* any
;; CEDET component (including EIEIO) gets activated by another 
;; package (Gnus, auth-source, ...).
					;(load-file "~/.emacs.d/vendor/cedet/cedet-devel-load.el")

;; Add further minor-modes to be enabled by semantic-mode.
;; See doc-string of `semantic-default-submodes' for other things
;; you can use here.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode t)
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode t)

;; Enable Semantic
(semantic-mode 1)

;; Enable EDE (Project Management) features
(global-ede-mode 1)

;; Configure arduino OS X dirs.
(setq ede-arduino-appdir "/Applications/Arduino.app/Contents/Resources/Java")

;;***********************************
;; ARDUINO
;;***********************************
(add-to-list 'load-path "~/.emacs.d/vendor/arduino-mode")
(setq auto-mode-alist (cons '("\\.\\(pde\\|ino\\)$" . arduino-mode) auto-mode-alist))
(autoload 'arduino-mode "arduino-mode" "Arduino editing mode." t)

;;***********************************
;; ORG 
;;***********************************
(defun org-hooks ()
  (lambda() 
    ('visual-line-mode 1)
    ('org-indent-mode 1)))

(add-hook 'org-mode 'org-hooks)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;***********************************
;; THEME
;;***********************************
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("cab317d0125d7aab145bc7ee03a1e16804d5abdfa2aa8738198ac30dc5f7b569" "13d20048c12826c7ea636fbe513d6f24c0d43709a761052adbca052708798ce3" "3f44e2d33b9deb2da947523e2169031d3707eec0426e78c7b8a646ef773a2077" "190a9882bef28d7e944aa610aa68fe1ee34ecea6127239178c7ac848754992df" "e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" default)))
 '(fci-rule-color "#383838")
 '(flymake-google-cpplint-command
   "/Library/Frameworks/Python.framework/Versions/3.6/bin/cpplint")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (flycheck-haskell scion highlight-indent-guides latex-preview-pane evil-mc intero csharp-mode flycheck markdown-mode md-readme ess ess-R-data-view auctex auctex-latexmk google-c-style flymake-google-cpplint iedit zenburn-theme yasnippet-snippets w3m w3 spotify sly-quicklisp slime racket-mode playerctl php-mode nodejs-repl node-resolver neotree multiple-cursors moz monochrome-theme moe-theme melancholy-theme madhat2r-theme impatient-mode haskell-snippets haskell-mode exec-path-from-shell evil-tutor evil-paredit dumb-jump company-irony common-lisp-snippets clojure-snippets auto-complete-c-headers arduino-mode ace-window ac-js2)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

