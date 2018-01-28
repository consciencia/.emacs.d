(setq DB-PATH "~/.emacs.d/.DB.data")
(setq DB-IMAGE (custom/map/create))

(defun custom/db/get (key)
  (custom/map/get key DB-IMAGE))

(defun custom/db/set (key val)
  (custom/map/set key val DB-IMAGE)
  (custom/serialize-to-file DB-IMAGE DB-PATH))

(if (file-exists-p DB-PATH)
    (setq DB-IMAGE (custom/parse-from-file DB-PATH))
  (custom/serialize-to-file DB-IMAGE DB-PATH))
