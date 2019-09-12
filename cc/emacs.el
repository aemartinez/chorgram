;; emacs -q --load emacs.el
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(define-key global-map [menu-bar] (make-sparse-keymap "menu-bar"))
;; Make a menu keymap (with a prompt string)
;; and make it the menu bar item's definition.
(define-key global-map [menu-bar file-menu]
  (cons "File" (make-sparse-keymap "File")))


(defun open-chorepgraphy ()
  (interactive)
  (find-file))

;; Define specific subcommands in this menu.
(define-key global-map
  [menu-bar file-menu backward]
  '("Run cc-closure" . open-chorepgraphy))
(define-key global-map
  [menu-bar file-menu project]
  '("Project" . open-chorepgraphy))
(define-key global-map
  [menu-bar file-menu forward]
  '("Open choreography..." . open-chorepgraphy))

;;(setq-default frame-title-format '("CHORGRAM %f"))
(setq-default frame-title-format '("CHORGRAM 0.2"))
(setq-default mode-line-format nil) 

;; inhibit-startup-echo-area-message MUST be set to a hardcoded 
;; string of your login name 
(setq inhibit-startup-echo-area-message "USERNAME")
(setq inhibit-startup-message t)

(setq  window-divider-default-places t)
(window-divider-mode 1)

(package-initialize)
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

(require 'use-package) ; guess what this one does too ?

(use-package hide-mode-line
  :init)


(find-file "../experiments/jlamp2020/ex3/global_view.sgg")
(read-only-mode 1)
(hide-mode-line-mode)

(split-window-below)
(other-window 1)
(find-file "proj/graph-2.png")
(hide-mode-line-mode)

(other-window 1)

(defun my-imenu-create-index ()
  (let ((index-alist '
         (("choreography" .
           (("sgg source" . 0)
            ("diagram" . 0)
            ("graph.ml" . 0)
            )
           )
          ("cc2-pom" .
           (("closure" .
             (("1" . (("diagram" . 0) ("graph.ml" . 0)))
              ("2" . (("diagram" . 0) ("graph.ml" . 0)))
              )
             )
            ("errors" .
             (("1" . (("diagram" . 0) ("graph.ml" . 0)))
              ("2" . (("diagram" . 0) ("graph.ml" . 0)))
              )
             )
            ))
          )))
    index-alist))


(defun my-imenu-init ()
  "Initialize `imenu' variables in current buffer."
  (setq-local imenu-create-index-function
              'my-imenu-create-index))

(my-imenu-init)

(use-package imenu-list
  :init
  (progn
    (setq imenu-list-position 'left)
    (imenu-list)
    (hide-mode-line-mode)
    ))

