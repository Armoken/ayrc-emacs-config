;; csharp-hs-forward-sexp.el
;;
;; csharp support for hideshow.el
;;
;; Sat, 12 Apr 2008  21:32
;;


(defun csharp-hs-forward-sexp (&optional arg)

  "I set hs-forward-sexp-func to this function.

I found this customization necessary to do the hide/show magic in C#
code, when dealing with region/endregion. This routine
goes forward one s-expression, whether it is defined by curly braces
or region/endregion. It handles nesting, too.

The forward-sexp method takes an arg which can be negative, which
indicates the move should be backward.  Therefore, to be fully
correct this function should also handle a negative arg. However,
the hideshow.el package never uses negative args to its
hs-forward-sexp-func, so it doesn't matter that this function does not
do negative numbers.

The arg can also be greater than 1, which means go forward
multiple times. This function doesn't handle that EITHER.  But
again, I haven't see that as a problem."

  (message "csharp-hs-forward-sexp, (arg %d) (point %d)..."
           (if (numberp arg) arg -1)
           (point))
  
  (let ((nestlevel 0)
        (mark1 (point))
        (done nil)
        )
    
    (if (and arg (< arg 0))
        (message "negative arg (%d) is not supported..." arg)

      ;; else, we have a positive argument, hence move forward.
      ;; simple case is just move forward one brace
      (if (looking-at "{")
          (forward-sexp arg)
        
        ; The more complex case is dealing with a "region/endregion" block.
        ; We have to deal with nested regions!
        (and
         (while (not done)
           (re-search-forward "^[ \\t]*#[ \\t]*\\(region\\|endregion\\)\\b"
                              (point-max) 'move)
           (cond
            
            ((eobp))                    ; do nothing if at end of buffer
            
            ((and
              (match-beginning 1)
              ;; if the match is longer than 6 chars, we know it is "endregion"
              (if (> (- (match-end 1) (match-beginning 1)) 6)
                  (setq nestlevel (1- nestlevel))
                (setq nestlevel (1+ nestlevel))
                )
              )))

           (setq done (not (and (> nestlevel 0) (not (eobp)))))
           
           )                            ; while
                        
         (if (= nest 0)
             (goto-char (match-end 2)))

         )
        )
      )
    )
  )


(unless (assoc 'csharp-mode hs-special-modes-alist)
          (push '(csharp-mode
                  ; "\\(^\\s*#\\s*region\\b\\)\\|{"      ; regexp for start block DID NOT WORK
                  "\\(^[ \\t]*#[ \\t]*region\\b\\)\\|{"  ; regexp for start block
                  
                  ; "\\(^\\s*#\\s*endregion\\b\\)\\|}"   ; regexp for end block NO WORKY!
                  "\\(^[ \\t]*#[ \\t]*endregion\\b\\)\\|}"   ; regexp for end block
                  
                  "/[*/]"                                ; regexp for comment start
                  
                  csharp-hs-forward-sexp                 ; hs-forward-sexp-func
                  hs-c-like-adjust-block-beginning       ;c-like adjust (1 char)
                  ;csharp-hs-adjust-block-beginning      ;csharp adjust ?
                  )
                hs-special-modes-alist))


;;
;; To use this, put this into your csharp-mode-hook:
;;
;;       ; for hide/show support
;;       (hs-minor-mode 1)
;;       (setq hs-isearch-open t)
;; 
;;       ; with point inside the block, use these keys to hide/show
;;       (local-set-key "\C-c>"  'hs-hide-block)
;;       (local-set-key "\C-c<"  'hs-show-block)



;;; end of csharp-hs-forward-sexp.el
