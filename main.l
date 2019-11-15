(app)

(action
   (html 0 "Case Conversion" "@lib.css" NIL
      (form NIL
         (<grid> 2
            "File"
            (gui '(+Able +UpField) '(not (: home obj jpg)) 30)
            (gui '(+Drop +Button) '(field -1)
              '(if (: home obj jpg) ,"Uninstall" ,"Install")
			  '(cond
				 ((: home obj jpg)
					(ask ,"Uninstall Picture?"
					   (put!> (: home top 1 obj) 'jpg NIL) ) )
				 ((: drop) (setq *FN (: drop))   T ) ) ) 
         ) 
       ) 
	(prinl "HELLO WORLD " *FN)
   )
)
