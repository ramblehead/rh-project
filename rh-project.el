;;; code-groups.el --- Minor mode for ramblehead projects management

;; rh-project support for Emacs
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Description: provides utility funtions to find project dir and root. Also
;; provides helper functions for project setup, compillation and running.
;;
;; Author: Victor Rybynok
;; Copyright (C) 2021, Victor Rybynok, all rights reserved.

;;; Code:

(require 'vterm nil t)

(defgroup rh-project nil
  "Ramblehead Project mode."
  :prefix "rh-project-"
  ;; :group '???
  )

(defvar rh-project-initialised-projects '())
(defvar rh-project-trusted-dir-marker ".rh-trusted")
(defvar rh-project-dir-name ".rh-project")

(defun rh-project-buffer-dir ()
  (or (and buffer-file-name
           (file-name-directory buffer-file-name))
      (and (or (eq major-mode 'compilation-mode)
               (eq major-mode 'dired-mode)
               ;; After some update lsp-mode in lsp-deps-providers plist calls
               ;; this function in fundamental-mode with buffer-file-name equal
               ;; nil. So, using the following check as a hack.
               ;; Probably, should switch to eglot.
               (eq major-mode 'fundamental-mode))
           default-directory)))

(defun rh-project-get-path ()
  (let* ((buffer-dir (rh-project-buffer-dir))
         (src-tree-root (and buffer-dir
                             (locate-dominating-file
                              buffer-dir
                              rh-project-dir-name))))
    (when src-tree-root
      (file-name-as-directory (concat src-tree-root rh-project-dir-name)))))

(defun rh-project-in-trusted-dir ()
  (locate-dominating-file
   (rh-project-buffer-dir)
   rh-project-trusted-dir-marker))

(defun rh-project-get-root ()
  (let ((rh-project (rh-project-get-path)))
    (when rh-project
      (abbreviate-file-name
       (expand-file-name (concat rh-project "../"))))))

(cl-defun rh-project-setup ()
  (let ((rh-project-path (rh-project-get-path)))
    (when rh-project-path
      (when buffer-file-name
        (message (concat "rh-project: " rh-project-path)))
      (let ((setup-file-path (concat rh-project-path "setup.el"))
            (init-file-path (concat rh-project-path "init.el"))
            (rh-project-id (directory-file-name
                            (expand-file-name rh-project-path))))
        (if (and (not (member rh-project-id rh-project-trusted-ids))
                 (not (rh-project-in-trusted-dir)))
            (message (concat "rh-project: '" rh-project-id
                             "' is not trusted. "
                             "Ignoring its 'init.el' and 'setup.el' files."))
          (when (and (file-exists-p init-file-path)
                     (not (member rh-project-id rh-project-initialised-projects)))
            (add-to-list 'rh-project-initialised-projects rh-project-id)
            (load init-file-path))
          (when (file-exists-p setup-file-path)
            (load setup-file-path nil t)))))))

(defun rh-project-compile (command compilation-buffer-or-name)
  (let* ((project-path (rh-project-get-path))
         (full-command (concat project-path command))
         (current-window (frame-selected-window))
         compilation-buffer)
    (when (get-buffer compilation-buffer-or-name)
      (with-current-buffer compilation-buffer-or-name
        (when (vterm-check-proc)
          (vterm-send-C-c)
          ;; Giving 1 sec for complication process to end; if it does not end the
          ;; following kill-buffer() should ask whether to kill it with live
          ;; process.
          (sit-for 1))
        (kill-buffer)))
    (setq compilation-buffer (generate-new-buffer compilation-buffer-or-name))
    (g2w-buffer-destination-window-init compilation-buffer current-window nil)
    (with-current-buffer compilation-buffer
      (vterm-mode)
      (setq-local rh-project-compile t)
      ;; (compilation-minor-mode)
      (vterm-send-string
       (concat "TIMEFORMAT=$'\\nTask took %Rs' && time "
               full-command
               "; exit"))
      (vterm-send-return))
    (rh-bs-display-buffer-in-bootom-0-side-window compilation-buffer)))

(defalias 'rh-project-run #'rh-project-compile)

(defun rh-project-term (term-buffer-or-name &optional pwd)
  (let* ((vterm-pwd (or pwd (rh-project-get-root)))
         (vterm-buffer (get-buffer term-buffer-or-name)))
    (if (and vterm-buffer (get-buffer-process vterm-buffer))
        (rh-bs-display-buffer-in-bootom-0-side-window vterm-buffer)
      (if vterm-buffer (kill-buffer vterm-buffer))
      (setq vterm-buffer (get-buffer-create term-buffer-or-name))
      (with-current-buffer vterm-buffer
        (setq-local vterm-kill-buffer-on-exit t)
        (setq-local default-directory vterm-pwd)
        (vterm-mode)))
    (rh-bs-display-buffer-in-bootom-0-side-window vterm-buffer)
    (select-window (rh-bs-get-bootom-0-side-window))))

(defun rh-project-run-shell-command (command output-buffer-or-name)
  (let* ((project-path (rh-project-get-path))
         (full-command (concat project-path command))
         output-buffer)
    (when (get-buffer output-buffer-or-name)
      (with-current-buffer output-buffer-or-name
        (when (process-live-p vterm--process)
          (vterm-send-C-c)
          ;; (sit-for 0.1)
          ;; (vterm-send-C-c)
          ;; (sit-for 1)
          (vterm-send-string "exit")
          (vterm-send-return)
          (sit-for 1))
        (kill-buffer)))
    (setq output-buffer (generate-new-buffer output-buffer-or-name))
    (with-current-buffer output-buffer
      (vterm-mode)
      ;; (compilation-minor-mode)
      (vterm-send-string full-command)
      (vterm-send-return))
    (rh-bs-display-buffer-in-bootom-0-side-window output-buffer)
    (get-buffer-process output-buffer)))

(defun rh-project-kill-shell-process
    (output-buffer-or-name &optional interrupt)
  (let ((buffer (get-buffer output-buffer-or-name)))
    (when (and buffer (not (get-buffer-window buffer 'visible)))
      (rh-bs-display-buffer-in-bootom-0-side-window buffer))
    (with-current-buffer buffer
      (when (process-live-p vterm--process)
        (vterm-send-C-c)
        (sit-for 0.1)
        (vterm-send-string "exit")
        (vterm-send-return)))))

(provide 'rh-project)
;;; code-groups.el ends here
