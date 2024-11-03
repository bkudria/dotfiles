installHookElisp = (key) ->
  "
    (progn
      (defun emacs-everywhere-expand-snippet ()
        (remove-hook 'emacs-everywhere-mode-hook #'emacs-everywhere-expand-snippet)
        (evil-insert-state 1)
        (insert \"#{key}\")
        (yas-expand)
      )
      (add-hook 'emacs-everywhere-mode-hook #'emacs-everywhere-expand-snippet 100)
      (emacs-everywhere)
    )
 "

emacsClient = (elisp) ->
  hs.task.new("/opt/homebrew/bin/emacsclient", nil, {"--eval", elisp})\start!

contains_yasnippet_syntax = (template_line) ->
  template_line\find("[^\\]%$") or template_line\find("[^\\]`")

trigger_result_for = (key, template_lines, emacs) ->
  trigger_result = if hs.fnutils.some(template_lines, contains_yasnippet_syntax)
    ->
      if emacs\isRunning!
        if emacs\isFocused!
          {key}
        else
          emacsClient(installHookElisp(key))
          {}
      else
        {"emacs not running"}
  else
    template_lines

  trigger_result

read_key_and_template_in = (file) ->
  lines = [line for line in io.lines(file)]

  key_pattern = "# key: (%w+)"
  key = hs.fnutils.find(lines, (line) -> line\match(key_pattern))\match(key_pattern)

  template_first = hs.fnutils.find(lines, (line) -> line\find("#", 1, true) != 1 and line != "")
  template_start = hs.fnutils.indexOf(lines, template_first)
  template_lines = [line for line in *lines[template_start,]]

  key, {:key, lines: template_lines}

snippets_path = "~/.hammerspoon/snippets"

files = hs.fnutils.split(hs.execute("find #{snippets_path}/ -type f")\gsub("\n$", ""), "\n")

templates = {read_key_and_template_in(file) for file in *files}

(emacs) ->
  hs.fnutils.map(templates, (template) -> trigger_result_for(template.key, template.lines, emacs))
