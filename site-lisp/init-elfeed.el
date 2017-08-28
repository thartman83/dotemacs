;;; init-elfeed.el --- Elfeed customizations and hooks
;; Copyright (c) 2016 Thomas Hartman (thomas.lees.hartman@gmail.com)

;;; Commentary:

;;; Code:

(setq elfeed-feeds
      '(
        ;; Programming
        "http://blog.nullspace.io/feed.xml"
        "https://codewords.recurse.com/feed.xml"
        "http://mcfunley.com/feed/rss"
        "http://eli.thegreenplace.net/archives/all"
        "http://www.evanjones.ca/index.rss"
;        "https://blog.lse.epita.fr/"
        "http://blog.jessitron.com/feeds/posts/default"
        "http://blog.regehr.org/feed"
        "https://www.snellman.net/blog/rss-index.xml"
        "http://jvns.ca/atom.xml"
        "http://maryrosecook.com/blog/feed"
        ))
(provide 'init-elfeed)
;;; init-elfeed.el ends here
