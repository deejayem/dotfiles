(prelude-require-package 'use-package)
(require 'use-package)

(setq use-package-always-ensure t)

(prelude-require-packages '(quelpa quelpa-use-package))
(use-package quelpa)
(use-package quelpa-use-package)

(provide 'init-use-package)

