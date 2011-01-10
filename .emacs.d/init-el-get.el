(setq-default
 el-get-sources
 (let ((el-get-recipes
        '(
          ack
          autofit-frame
          autopair
          coffee-mode
          dtrt-indent
          el-get
          elunit
          emacs-textmate
          fit-frame
          flymake-ruby
          flymake-fringe-icons
          git-blame
          git-modeline
          haml-mode
          haskell-mode
          highlight-parentheses
          hs-lint
          ido-hacks
          lua-mode
          magit
          markdown-mode
          notify
          org-mode
          php-mode-improved
          rinari
          ruby-block
          rvm
          sass-mode
          scala-mode
          smart-tab
          smarttabs
          smex
          sml-mode
          smooth-scroll
          smooth-scrolling
          sudo-save
          ))
       (elpa-packages
        (mapcar (lambda (package) `(:name ,package :type elpa))
                '(
                  css-mode
                  diff-git
                  find-file-in-project
                  fringe-helper
                  gist
                  highlight-symbol
                  highline
                  light-symbol
                  ruby-electric
                  smart-operator
                  ))))
   (append el-get-recipes elpa-packages)))

(el-get 'sync)