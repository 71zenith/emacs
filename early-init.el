(setq-default package-enable-at-startup nil
              use-dialog-box nil
              file-name-handler-alist nil
              warning-minimum-level :error
              use-package-always-ensure t)

(defun display-startup-echo-area-message ()
  (message ""))

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1024 1024 20)
                  gc-cons-percentage 0.2)))

(setq-default default-frame-alist
              '((tool-bar-lines . 0)
                (menu-bar-lines . 0)
                (undecorated . t)
                (vertical-scroll-bars . nil)
                (horizontal-scroll-bars . nil)))


(load-theme 'base16-stylix-theme t)
