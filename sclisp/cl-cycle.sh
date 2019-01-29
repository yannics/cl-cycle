#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

dirsbcl='/opt/local/bin/sbcl'
if [ ! -e $dirsbcl ]
then dirsbcl='/usr/local/bin/sbcl'
if [ ! -e $dirsbcl ]
then dirsbcl='/usr/bin/sbcl'
if [ ! -e $dirsbcl ]
then dirsbcl=`pwd`/sbcl
if [ ! -e $dirsbcl ]
then rm $DIR/.tmp_array
exit 1
fi
fi
fi
fi

cat $DIR/.tmp_array | sed -e "s/\[/\(/g;s/\]/\)/g;s/,/\ /g" > $DIR/.tmp_list

echo "(load \"~/.sbclrc\")
(require 'cl-cycle)
(in-package :cl-cycle)

(defun FSC (lst &key name dir)
  (with-open-file (file-stream (make-pathname :directory (pathname-directory dir)
					      :name \".tmp_\"
					      :type \"scd\")
			       :direction :output
			       :if-exists :supersede
			       :if-does-not-exist :create)
    (labels ((convert-list-to-array (lst)
	       (with-output-to-string (stream)
		 (uiop:run-program (concatenate 'string \"echo '\" (format nil \"~a\" lst) \"' | sed -e 's/\\ /, /g;s/(/[ /g;s/)/ ]/g' | awk '{print tolower(\$0)}'\") :output stream))))
      (format file-stream \"~a = ~a\" name (convert-list-to-array lst)))))

(defun read-file (file)
  (mapcar #'read-from-string
	  (with-open-file (in-stream file
				     :direction :input
				     :element-type 'character)
	    (loop with length = (file-length in-stream)
	       while (< (file-position in-stream) length)
	       collect (read-line in-stream)))))" > $DIR/.tmp_.lisp

echo "(defparameter foo (read-file \"$DIR/.tmp_list\"))" >> $DIR/.tmp_.lisp

echo "(fsc (apply (cadr foo) (caddr foo)) :name (string-downcase (string (car foo))) :dir \"$DIR/\")" >> $DIR/.tmp_.lisp

$dirsbcl --script $DIR/.tmp_.lisp

cat $DIR/.tmp_.scd | sed -e "s/\ ,//g" > $DIR/.tmp.scd

if [ -s $DIR/.tmp.scd ]
then
  echo ";" >> $DIR/.tmp.scd

  # reduce-scd-file
  cat $DIR/.tmp.scd | sed 's/  */\ /g' > $DIR/.tmp_.scd
  cat $DIR/.tmp_.scd | tr "\\n" " " | sed 's/  /\ /g' > $DIR/.tmp.scd

else
  line=$(head -n 1 $DIR/.tmp_array)
  echo "$line = \"WARNING: Something wrong ... check if cl-cycle is well installed.\"" >> $DIR/.tmp.scd
fi

rm $DIR/.tmp_*
