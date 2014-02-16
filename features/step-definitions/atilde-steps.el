(When "^I add \\(.+\\) element to \\(.+\\) list$"
  "Add given element to given list"
  (lambda (element lst)
    (add-to-list (intern lst) (read element))))
