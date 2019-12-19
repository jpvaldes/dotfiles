(setq url-proxy-services
   '(("no_proxy" . "^\\(localhost\\|10.*\\)")
     ("http" . "149.203.231.111:4711")
     ("https" . "149.203.231.111:4711")))

(provide 'init-proxy)
