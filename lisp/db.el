(defun custom/parse-from-file (path)
  (read (f-read-text path 'utf-8)))

(defun custom/serialize-to-file (obj path)
  (f-write-text (prin1-to-string obj) 'utf-8 path))

(defun custom/db/get (key)
  (custom/map/get key DB-IMAGE))

(defun custom/db/set (key val)
  (custom/map/set key val DB-IMAGE)
  (custom/serialize-to-file DB-IMAGE DB-PATH))

(defun custom/db/projectile/get (key)
  (custom/map/get key (custom/db/get "PROJECTILE")))

(defun custom/db/projectile/set (key val)
  (custom/map/set key val (custom/db/get "PROJECTILE"))
  (custom/serialize-to-file DB-IMAGE DB-PATH))



(setq DB-PATH "~/.emacs.d/.DB.data")
(setq DB-IMAGE (custom/map/create))
(custom/map/set "PROJECTILE" (custom/map/create) DB-IMAGE)
(if (file-exists-p DB-PATH)
    (setq DB-IMAGE (custom/parse-from-file DB-PATH))
  (custom/serialize-to-file DB-IMAGE DB-PATH))
