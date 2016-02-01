In SBCL 1.3.1 on an El-Capitan mac, consider this use of `case`:

    (case 42 ((42) t))
     > t

Now define a constant for `42`:

    (defconstant foobey 42)

`Case` doesn't seem to work on it any more

    (case 42 ((foobey) t))
     > nil

I can't find any difference between 42 and the constant

    (class-of 42)
     > #<BUILT-IN-CLASS COMMON-LISP:FIXNUM>
    (class-of foobey)
     > #<BUILT-IN-CLASS COMMON-LISP:FIXNUM>

The [documentation for `case`][1] doesn't seem to indicate any special treatment
for `keys` other than `t` and `otherwise`. Digging under [the hyperlink for the
word "same,"][2] I find that `eql` might be the predicate used by `case`, but

    (eql foobey 42)
     > t

Even if only `eq` is used, I still get

    (eq foobey 42)
     > t

What is my misunderstanding of `case`?  Is this a bug in SBCL 1.3.1?

  [1]: http://www.lispworks.com/documentation/HyperSpec/Body/m_case_.htm
  [2]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_s.htm#same