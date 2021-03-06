;;; bufhistory.el

;; Author: Koji Mitsuda <fbkante2u atmark gmail.com>
;; Keywords: convenience
;; Version: 0.91

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; To use bufhistory, just add the following code into your .emacs:
;;
;;    (require 'bufhistory)
;;    (bufhistory-mode 1)
;;

;; 注意 bufhistory-styleを実行時に変更した際には、bufhistory-modeを一旦無効にし、再び有効にする必要がある

;; version 0.91
;; 負荷軽減のためheader-line更新のたびに文字列を計算するのをやめた。
;; adviceではなく kill-buffer-hook を使うようにした。

(eval-when-compile (require 'cl))

;;バッファリストの表示最大値 9以下の値を指定すべし。
(defvar bufhistory-maxindex 6)

;;バッファリストの表示先
(defvar bufhistory-style 'headerline)
;;(defvar bufhistory-style 'echoarea)
;;(defvar bufhistory-style 'frametitle)
;;(defvar bufhistory-style 'modeline)
;;(defvar bufhistory-style nil)

(defconst bufhistory-header-line-format '(:eval (bufhistory-get-epigraph)))

;;frame-title-formatを保存する
(defvar bufhistory-frame-title-format nil)
(defvar bufhistory-mode-line-format nil)

;;バッファローカルにウィンドウ毎のポイント位置を記録する。
(defvar bufhistory-point-hash nil)
(make-variable-buffer-local 'bufhistory-point-hash)

;;delete bufferを取り除いたバッファ履歴を返す
(defun bufhistory-get-history (win)
  (let ((bl (window-parameter win 'bufhistory)))
    (delq nil (mapcar (lambda (b) (if (buffer-live-p b) b nil)) bl))))

;;念の為更新した後にウィンドウの履歴リストを返す
(defun bufhistory-update (&optional window)
  (let* ((w (or window (selected-window)))
         (b (window-buffer w))
         (history (bufhistory-get-history w)))
    (unless (eq b (car history))
      (setq history (cons b (delq b history)))
      (set-window-parameter w 'bufhistory history)
      (set-window-parameter w 'bufhistory-epigraph (bufhistory-calc-epigraph history)))
    history))

;;ウィンドウの履歴文字列を取り出す
(defun bufhistory-get-epigraph (&optional win)
  (window-parameter (or win (selected-window)) 'bufhistory-epigraph))

;;履歴リストから履歴文字列を作成する
(defun bufhistory-calc-epigraph (history)
  (let* ((hs (length history))
	 (maxindex bufhistory-maxindex)
         (i (min (1+ maxindex) hs 10))
         (result nil))
    (when (< i hs) (push (format "-:%s" (car (last history))) result))
    (while (> i 1)
      (setq i (1- i))
      (push (format "%d:%s" i (nth i history)) result))
    (mapconcat 'identity result " ")))

;;現状のバッファリストをエコーエリアに表示する
(defun bufhistory-echo ()
  (when (eq bufhistory-style 'echoarea)
    (let ((message-log-max)) (message (bufhistory-get-epigraph)))))

;;現在選択されているバッファをリスト末尾に回す
(defun bufhistory-rotate-buffer ()
  (interactive)
  (let* ((w (selected-window))
         (history (bufhistory-update w)))
    (when (> (length history) 1)
      (let* ((head (car history))
             (tail (cdr history))
	     (newhistory (append tail (list head))))
        (set-window-parameter w 'bufhistory newhistory)
	(set-window-parameter w 'bufhistory-epigraph (bufhistory-calc-epigraph newhistory))
        (switch-to-buffer (car tail))
        (bufhistory-echo)))))

;;念のためバッファ履歴を更新して、履歴文字列を返す。
(defun bufhistory-argument ()
  (let ((w (selected-window)))
    (bufhistory-update w)
    (bufhistory-get-epigraph w)))

;;バッファ選択
(defun bufhistory-select-buffer (ch &optional quiet-p)
  (interactive (list (read-char (format "Select Buffer (%s):" (bufhistory-argument)))))
  (let* ((win (selected-window))
         (history (bufhistory-update win))
         (b))
    (if (equal ch ?0)
        (bufhistory-rotate-buffer)
      (if (equal ch ?-)
          (setq b (car (last history)))
        (let ((index (- ch ?0))
              (hmax (min (1+ bufhistory-maxindex) (length history) 10)))
          (when (and (> index 0) (< index hmax)) (setq b (nth index history))))))
    (when b
      (switch-to-buffer b)
      (unless quiet-p (bufhistory-echo)))))

(defun bufhistory-select-buffer-1 () (interactive) (bufhistory-select-buffer ?1))
(defun bufhistory-select-buffer-2 () (interactive) (bufhistory-select-buffer ?2))
(defun bufhistory-select-buffer-3 () (interactive) (bufhistory-select-buffer ?3))
(defun bufhistory-select-buffer-4 () (interactive) (bufhistory-select-buffer ?4))
(defun bufhistory-select-buffer-5 () (interactive) (bufhistory-select-buffer ?5))
(defun bufhistory-select-buffer-6 () (interactive) (bufhistory-select-buffer ?6))
(defun bufhistory-select-buffer-7 () (interactive) (bufhistory-select-buffer ?7))
(defun bufhistory-select-buffer-8 () (interactive) (bufhistory-select-buffer ?8))
(defun bufhistory-select-buffer-9 () (interactive) (bufhistory-select-buffer ?9))
;;バッファ履歴末尾を選択する。
(defun bufhistory-last-buffer () (interactive) (bufhistory-select-buffer ?-))

;;能動的なバッファの切り替えを検知する。ポイントを動かす
(defadvice switch-to-buffer (after bufhistory-switch-to-buffer)
  (bufhistory-update)
  (when bufhistory-point-hash
    (let ((p (gethash (selected-window) bufhistory-point-hash)))
      (when p (goto-char p)))))

(defadvice pop-to-buffer (after bufhistory-pop-to-buffer)
  (bufhistory-update))

(defun bufhistory-kill-buffer-hook ()
  (let ((buffer (current-buffer)))
    ;;削除バッファの代替をしてしまう。
    (dolist (win (get-buffer-window-list buffer nil 'visible))
      (with-selected-window win (bufhistory-select-buffer ?1 t)))
    ;;削除バッファを全ての履歴リストから消去する
    (dolist (frame (frame-list))
      (dolist (win (window-list frame))
	(let ((history (delq buffer (window-parameter win 'bufhistory))))
	  (set-window-parameter win 'bufhistory history)
	  (set-window-parameter win 'bufhistory-epigraph (bufhistory-calc-epigraph history)))))))

;;新たにウィンドウを作成した時に、バッファ履歴とポイント位置をコピーする
(defun bufhistory-copy-history (window-dest window-src)
  (let* ((history (bufhistory-get-history window-src))
	 (epigraph (bufhistory-calc-epigraph history)))
    (set-window-parameter window-dest 'bufhistory history)
    (set-window-parameter window-dest 'bufhistory-epigraph epigraph))
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when bufhistory-point-hash
        (let ((p (gethash window-src bufhistory-point-hash)))
          (when p (puthash window-dest p bufhistory-point-hash)))))))

(defadvice split-window (around bufhistory-split-window)
  ad-do-it
  (let* ((arg0 (ad-get-arg 0))
         (base (or arg0 (selected-window)))
         (created ad-return-value))
    (bufhistory-copy-history created base)))

;;ポイント位置を保存する
(defun bufhistory-pre-command ()
  ;;こうしないとハッシュテーブルがバッファローカルにならない。
  (unless bufhistory-point-hash (setq bufhistory-point-hash (make-hash-table :test 'eq)))
  ;;本当は(window-start)も保存すべき
  (puthash (selected-window) (point) bufhistory-point-hash))

;;popwin対策。新しくウィンドウを作成した時に、複製元のバッファ履歴をコピーする
(defadvice popwin:create-popup-window (after bufhistory-create-popup-window)
  (destructuring-bind (master-win popup-win win-map) ad-return-value
    (dolist (pair win-map)
      (bufhistory-copy-history (cdr pair) (car pair)))))

;;popwin対策。ウィンドウ構造を変更する時には、新規ウィンドウを起点とする
(defadvice popwin:last-selected-window (after bufhistory-last-selected-window)
  (setq ad-return-value (split-window (frame-first-window))))

(defun bufhistory-enable ()
  (ad-enable-advice 'split-window 'around 'bufhistory-split-window)
  (ad-enable-advice 'pop-to-buffer 'after 'bufhistory-pop-to-buffer)
  (ad-enable-advice 'switch-to-buffer 'after 'bufhistory-switch-to-buffer)
  (ad-activate 'split-window)
  (ad-activate 'pop-to-buffer)
  (ad-activate 'switch-to-buffer)
  (add-hook 'pre-command-hook 'bufhistory-pre-command)
  (add-hook 'kill-buffer-hook 'bufhistory-kill-buffer-hook)
  ;;popwin対策
  (when (featurep 'popwin)
    (ad-enable-advice 'popwin:create-popup-window 'after 'bufhistory-create-popup-window)
    (ad-enable-advice 'popwin:last-selected-window 'after 'bufhistory-last-selected-window)
    (ad-activate 'popwin:create-popup-window)
    (ad-activate 'popwin:last-selected-window))

  (when (eq bufhistory-style 'headerline)
    (setq-default header-line-format bufhistory-header-line-format)
    (dolist (b (buffer-list))
      (with-current-buffer b (setq header-line-format bufhistory-header-line-format))))
  (when (eq bufhistory-style 'frametitle)
    (setq bufhistory-frame-title-format frame-title-format)
    (setq frame-title-format (list bufhistory-frame-title-format "   " bufhistory-header-line-format)))
  (when (eq bufhistory-style 'modeline)
    (setq bufhistory-mode-line-format (default-value 'mode-line-format))
    (setq-default mode-line-format (list bufhistory-mode-line-format " " bufhistory-header-line-format))))

(defun bufhistory-disable ()
  (ad-disable-advice 'split-window 'around 'bufhistory-split-window)
  (ad-disable-advice 'pop-to-buffer 'after 'bufhistory-pop-to-buffer)
  (ad-disable-advice 'switch-to-buffer 'after 'bufhistory-switch-to-buffer)
  (ad-activate 'split-window)
  (ad-activate 'pop-to-buffer)
  (ad-activate 'switch-to-buffer)
  (remove-hook 'pre-command-hook 'bufhistory-pre-command)
  (remove-hook 'kill-buffer-hook 'bufhistory-kill-buffer-hook)
  (when (featurep 'popwin)
    (ad-disable-advice 'popwin:create-popup-window 'after 'bufhistory-create-popup-window)
    (ad-disable-advice 'popwin:last-selected-window 'after 'bufhistory-last-selected-window)
    (ad-activate 'popwin:create-popup-window)
    (ad-activate 'popwin:last-selected-window))
  (when (eq (default-value 'header-line-format) bufhistory-header-line-format)
    (setq-default header-line-format nil))
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (eq header-line-format bufhistory-header-line-format) (setq header-line-format nil))))
  (when (eq bufhistory-style 'frametitle)
    (setq frame-title-format bufhistory-frame-title-format))
  (when (eq bufhistory-style 'modeline)
    (setq-default mode-line-format bufhistory-mode-line-format)))

(defvar bufhistory-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-1") 'bufhistory-select-buffer-1)
    (define-key map (kbd "C-2") 'bufhistory-select-buffer-2)
    (define-key map (kbd "C-3") 'bufhistory-select-buffer-3)
    (define-key map (kbd "C-4") 'bufhistory-select-buffer-4)
    (define-key map (kbd "C-5") 'bufhistory-select-buffer-5)
    (define-key map (kbd "C-6") 'bufhistory-select-buffer-6)
    (define-key map (kbd "C-7") 'bufhistory-select-buffer-7)
    (define-key map (kbd "C-8") 'bufhistory-select-buffer-8)
    (define-key map (kbd "C-9") 'bufhistory-select-buffer-9)
    (define-key map (kbd "C--") 'bufhistory-last-buffer)
    (define-key map (kbd "C-0") 'bufhistory-rotate-buffer)
    map))

(define-minor-mode bufhistory-mode
  "bufhistory mode"
  :global t
  :keymap bufhistory-map
  (if bufhistory-mode (bufhistory-enable) (bufhistory-disable)))

(provide 'bufhistory)
