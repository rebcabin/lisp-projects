Following the documentation for [WITH-OUTPUT-TO-STRING][1] and
[GET-OUTPUT-STREAM-STRING][2], I expect the following to work, and they do:

    (print
     (with-output-to-string (sb nil)
       (format sb "~A " "hello, ")
       (format sb "~A~&" "world")
       sb))
    
    (print
     (let ((sb (make-string-output-stream)))
       (format sb "~A " "hello, ")
       (format sb "~A~&" "world")
       (get-output-stream-string sb)))

However, the following, which is close to one of the examples in
[WITH-OUTPUT-TO-STRING][1], does not:

    (print
     (with-output-to-string (sb (make-array
                                 '(0)
                                 :element-type 'base-char
                                 :fill-pointer 0
                                 :adjustable t))
       (format sb "~A " "hello, ")
       (format sb "~A~&" "world")
       sb))

producing, instead, the output stream, itself, rather than the string held within:

    #<SB-IMPL::FILL-POINTER-OUTPUT-STREAM {1005FBE523}>

I have not been able to find a way to extract the string inside the output
stream. I suspect that it has something to do with dynamic extent, but my
understanding falters, here. 

Obviously, I have kosher ways of achieving the desired result, so I am merely
curious to discover my misunderstanding of the language.

Because the documentation says that the results are undefined for
GET-OUTPUT-STREAM-STRING on a stream _not_ created by MAKE-STRING-OUTPUT-STREAM, I
am not surprised that the following doesn't work:

    (print
     (with-output-to-string (sb (make-array
                                 '(0)
                                 :element-type 'base-char
                                 :fill-pointer 0
                                 :adjustable t))
       (format sb "~A " "hello, ")
       (format sb "~A~&" "world")
       (get-output-stream-string sb)))

but I'd still be grateful for finding a way of extracting the string in my third
example.

[1]: http://www.lispworks.com/documentation/HyperSpec/Body/m_w_out_.htm
[2]: http://www.lispworks.com/documentation/HyperSpec/Body/f_get_ou.htm