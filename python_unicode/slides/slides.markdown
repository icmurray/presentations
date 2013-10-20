# Handling Unicode in Python

---

# Overview

1. History lesson.
2. Python and unicode.
3. Common problems / pitfalls.
4. Best practice.

---

# In the beginning there was the Word...

---

# and the Word was in ASCII

---

# ASCII

* 7-bit
* 0 - 31 were control characters.
* 32 - 127 were printable characters.

---

# ASCII

    Decimal | Hex  | Character
    ------- | ---- | ---------
    65      | x41  | A
    49      | x31  | 1
    13      | x0D  | CR

---

# What to do with that extra bit?

---

# Anything! It was a free-for-all!

---

# ASCII

On some machines, it was used to represent characters necessary in other
languages that ASCII didn't provide. eg accented characters in French say, é.
Or Greek characters such as Σ

Some people, apparantly, set it on the last letter of a word!

---

# ANSwer?

---

# ANSI

---

# ANSI

* 0 - 128 standardised (similar to ASCII)
* 128 - 255 had many standards, called code pages.

---

# ANSI

This worked unless you wanted to read both Russian and Greek in the same file;
or share a Russian file with a Greek colleague.

---

# Unicode

---

# Codepoints

---

# Codepoints

Unicode defines a unique **code point** for every character out there.

The concept of a code point is **independant** of its representation.

---

# Codepoints

A

    U+0041
    LATIN CAPITAL LETTER A

ß

    U+00DF
    LATIN SMALL LETTER SHARP S

λ

    U+03BB
    GREEK SMALL LETTER LAMBDA

🐵

    U+01F435
    MONKEY FACE

---

# Unicode

* Originally the range was defined over `U+0000` to `U+FFFF`

* Modern unicode range in `U+000000` -> `U+10FFFF`

---

# Unicode

* Broken down into *planes*.
  * `U+0000` -> `U+FFFF` is the *Basic Multilingual Plane* (BMP).

* Not every code point in the range has been assigned a character.

* Some ranges are reserved.

---

# Encodings

---

# Encodings

Encodings transform a sequence of code points into a sequence of bytes, and back
again.  In order that code points can be represented in memory, on disk,
transferred over a socket, etc...

                 encode          decode
    [CodePoint] -------> [Byte] -------> [CodePoint]

---

# Encodings

Number of possible encodings, each with advanatages and disadvantages.

It's **not** a requirement of an encoding to be able to encode the full
unicode range.  In fact many don't.

                | A            | ß            | λ            | 🐵          
    ----------- | ------------ | ------------ | ------------ | -----------
    codepoint   | U+0041       | U+00DF       | U+03BB       | U+01F435
    ascii       | 41           |              |              | 
    latin-1     | 41           | df           |              |  
    ucs2        | 00 41        | 00 df        | 03 bb        |  
    utf-16 (be) | 00 41        | 00 df        | 03 bb        | d8 3d dc 35
    utf-32 (be) | 00 00 00 41  | 00 00 00 df  | 00 00 03 bb  | 00 01 f4 35
    utf-8       | 41           | c3 9f        | ce bb        | f0 9f 90 b5
    shift-jis   | 41           |              | 83 c9        | 

---

# UTF-32

* Fixed width (4 bytes)
* Covers whole range of unicode code points
* Fixed width means quick access to n-th character in the string
* Obviously, not space efficient
* Not backward-compatible with ascii
* Endianess: Byte Order Mark (BOM)
  * `U+FEFF` code unit precedes the first coded value.
  * If the endianess of the encoder matches the endianess of the decoder, then
    the decoder will detect the `U+FEFF` value.  Otherwise, it will detect the
    invalid `U+FFEF` value.

---

# UTF-16

* Variable width (2 bytes or 4 bytes)
* Covers almost whole range of unicode code points.
  * 1,112,064 code points cf. 1,114,112 space of unicode
* Utilises 2 reserved spaces in the BMP to encode code-points from outside
  the BMP.
  * `U+D800` to `U+DBFF` known as the high-surrogate code points
  * `U+DC00` to `U+DFFF` known as the low-surrogate code points
* Endianess: Use a BOM (same as UTF-32)

---

# UTF-8

* Variable length encoding: 1,2,3 and 4 byte wide values.
* Designed for backward compatibility with ASCII.
* No endianess to worry about.
  * Although, some windows programs add some bytes to the beginning of documents
    saved as UTF-8 in order to help automatic identification.
  * Not required nor recommended.recommended.
* Encodes whole unicode code point space.
* Character boundaries are easily found.
* Not efficient indexing: can't jump directly to the n-th code point.

---

# Python

---

# Python

How does this all fit in with Python?

---

# Python 2

* `str` type
* `unicode` type.
* Internally: 16-bit or 32-bit unsigned integer.  Depends upon compilation flag
  used.
  - 16 bit: ucs-2
  - 32-bit: ucs-4 (utf-32)

---

# The `unicode` type

The constructor's signature:

    !python
    unicode(string[, encoding, errors])

* Each argument should be an 8-bit string.
* The first argument is converted to Unicode using the given `encoding`.
* If no `encoding` is given, then `"ascii"` is assumed.
* `errors` argument takes three possible values:
  * `"strict"` (default)
  * `"replace"`
  * `"ignore"`

---

# The `unicode` type

    !python
    >>> unicode('a string')
    u'a string'

    >>> unicode('a lambda in utf-8: \xce\xbb', 'utf-8')
    u'a lambda in utf-8: \u03bb'

    >>> print unicode('a lambda in utf-8: \xce\xbb', 'utf-8')
    a lambda in utf-8: λ

---

# The `unicode` type

    !python
    >>> sigma = u'\u03a3'
    >>> print sigma
    Σ

    >>> print sigma.lower()
    σ
    
    >>> sigma.isupper()
    True

    >>> sigman.islower()
    False

---

# The `unicode` type

    !python
    >>> u'roses' < u'roset'
    True

    >>> u'rosés' < u'roset'
    False

[Unique Collation Algorithm](http://unicode.org/reports/tr10/) (UTA) specifies
that these should both be `True`.

---

# Converting between unicode and str

    !python
    >>> s = unicode('lambda: \xce\xbb', 'utf-8')
    >>> s.encode('utf-8')
    'lambda: \xce\xbb'

    >>> s.encode('utf-16')
    '\xff\xfel\x00a\x00m\x00b\x00d\x00a\x00:\x00 \x00\xbb\x03'

    >>> s.encode('utf-32')
    '\xff\xfe\x00\x00l\x00\x00\x00a\x00\x00\x00m\x00\x00\x00b\x00\x00\x00d\x00\x00\x00a\x00\x00\x00:\x00\x00\x00\x00\x00\x00\xbb\x03\x00\x00'

    >>> # And back again with `str.decode`.
    >>> as_shift_jis = s.encode('shift-jis')
    >>> s2 = as_shift_js.decode('shift-jis')
    >>> s == s2

---

# Unicode literals

    !python
    >>> print u'a unicode object'
    a unicode object
    
    >>> print u'a lambda: \u03bb'
    a lambda: λ

    >>> print u'a monkey face: \U0001F435'
    a monkey face: 🐵

---

# Common errors

    !python
    #!/usr/bin/python

    print "£"

Running the above file:

    ./test_unicode.py

    File "./test_unicode.py", line 3
    SyntaxError: Non-ASCII character '\xc2' in file ./unicode.py on line 3, but
    no encoding declared; see http://www.python.org/peps/pep-0263.html for
    details

---

# Common errors

£

    U+00A3
    POUND SIGN

    UTF-8:  0xC2 0xA3
    UTF-16: 0x00A3

In decimal, `0xC2 == 194`

---

# Solution 1

Encode the pound sign character with it's code-point:

    !python
    >>> print u'\u00A3'
    £

---

# Solution 2

Tell python what was used by your text editor to encode the file.  In this case
it was `UTF-8`:

    !python
    #!/usr/bin/python
    # -*- coding: utf-8 -*-

    print '£'

---

# Common errors

    !python
    #!/usr/bin/python
    # -*- coding: utf-8 -*-

    class MyClass(object):

        def __init__(self, my_value):
            self.my_value = my_value

        def __str__(self):
            return "My string value is: " + self.my_value

    o = MyClass(u'\u00A3')
    print o

---

# Solution

* The return value of `__str__()` must be a `str`.
* Normally a `TypeError` is raised (try returning an integer for example).
* In the case of a `unicode` being returned, the interpretter attempts to cast
  it to a `str` by encoding it with the default encoding (ascii).

---

# Solution

    !python
    #!/usr/bin/python
    # -*- coding: utf-8 -*-

    class MyClass(object):

        def __init__(self, my_value):
            self.my_value = my_value

        def __str__(self):
            return "My string value is: " + self.my_value.encode('utf-8')

        def __unicode__(self):
            return u"My value is:" + self.my_value

    o = MyClass(u'\u00A3')
    print o
    print unicode(o)

---

The general solution to not having these problems is to deal solely with
`unicode` objects when handling strings internally.

Convert from known encodings on input.  These may be user-specified, or may be
known before hand.  It's possible to guess by inspection, but it's not reliable.

Convert to a known encoding on the output.

Perform validation on the **decoded** unicode, not the **encoded** string.

---

# Input/output

    !bash
    cat my_utf8_file.txt
    Ooh ooh ooh 🐵

And this python script:

    !python
    #!/usr/bin/python
    # -*- coding: utf-8 -*-

    line = open('my_utf8_file.txt', 'r').readline().strip()
    print len(line)
    print type(line)
    print line.find(u'🐵')

Running it outputs:

    17
    <type 'str'>
    Traceback (most recent call last):
      File "./unicode.py", line 25, in <module>
        print line.find(u'🐵')
    UnicodeDecodeError: 'ascii' codec can't decode byte 0xf0 in position 12:
    ordinal not in range(128)

---

# Solution

    !python
    #!/usr/bin/python
    # -*- coding: utf-8 -*-

    import codecs

    line = codecs.open('my_utf8_file.txt', 'r', encoding='utf-8').readlines().strip()
    print len(line)
    print type(line)
    print line.find(u'🐵')

Outputs:

    13
    <type 'unicode'>
    12

---

# Similarly, for writing

    !python
    f = codecs.open('my_utf8_file', 'w', encoding='utf-8')
    f.write(u'\u00A3')
    f.seek(0)   # seeks correctly
    f.close()

---

# Here's a trick question:

Given the above, what does this print:

    !python
    >>> # Single-length "λ"
    >>> s = u'\u03bb'.encode('utf-16')
    >>> s2 = s + s
    >>> len(s2.decode('utf-16'))

---

# Answer
    
    !python
    >>> # Single-length "λ"
    >>> s = u'\u03bb'.encode('utf-16')
    >>> s2 = s + s
    >>> len(s2.decode('utf-16'))
    3

* Each decoding produced a BYte Order Mark!

---

# Some cool stuff I didn't know before writing these slides

Python has a `unicodedata` module which provides access to the Unicode Character
Database.  This defines various properties for all the Unicode characters.

    !python
    >>> import unicodedata
    >>> pound = unicodedata.lookup('POUND SIGN')
    >>> print pound
    £

    >>> seven = u'\u2166'
    >>> print seven
    Ⅶ

    >>> unicodedata.name(seven)
    'ROMAN NUMERAL SEVEN'

    >>> unicodedata.numeric(seven)
    7.0

---

# Any questions?

---

# References

* [Python unicode docs](http://docs.python.org/howto/unicode.html)
* [Joel on software on unicode](http://www.joelonsoftware.com/articles/Unicode.html)

