(module testsrfi74.scm
   (library btest srfi74)
   (main main))

(define +float-epsilon+ 1.19209290E-07)
(define +double-epsilon+ 2.2204460492503131e-16)

;;;; test utilities
(define (make-random-blob size)
   (let ((res (make-blob size)))
      (do ((i 0 (+ i 1)))
          ((= i size) res)
          (blob-u8-set! res i (random 256)))))

;; return an integer from - 2^((8*bytes)-1) to 2^((8*bytes)-1) - 1
(define (make-random-signed-int bytes)
   (let* ((positive?  (= (random 2) 0))
          (maxabsval  (exptbx #z2  (fixnum->bignum  (- (* 8 bytes) 1))))
          (val (randombx (if negative? (+bx maxabsval #z1) maxabsval))))
      (if positive?
          val
          (-bx #z0 val))))

(define (make-random-unsigned-int bytes)
   (let ((maxval (exptbx #z2 (fixnum->bignum (* 8 bytes)))))
      (randombx maxval)))

(define (make-random-real max)
   (* max (randomfl)))

(define (make-random-u8-list size)
   (do ((i 0 (+ i 1))
        (res '() (cons (random 256) res)))
       ((= i size) res)))

(define (make-random-uint-list bytes size)
   (do ((i 0 (+ i 1))
        (res '() (cons (make-random-unsigned-int bytes) res)))
       ((= i size) res)))

(define (make-random-sint-list bytes size)
   (do ((i 0 (+ i 1))
        (res '() (cons (make-random-signed-int bytes) res)))
       ((= i size) res)))

(define (make-random-real-list max size)
   (do ((i 0 (+ i 1))
        (res '() (cons (make-random-real max) res)))
       ((= i size) res)))

(define (=f32 a::float b::float)
   (let ((diff (abs (- a b)))
         (m (max  (abs a) (abs b))))
      (<= diff (* m +float-epsilon+))
      ))

(define (=f64 a::double b::double)
   (let ((diff (abs (- a b)))
         (m (max  (abs a) (abs b))))
      (<= diff (* m +double-epsilon+))
      ))
;;; 32bit floats should have ~7 decimal digits of precision
(define-macro (assert=f32 exp val)
   `(assert-predicate =f32  ,exp ,val))

;;; 64bit floats should have ~15 decimal digits of precision
(define-macro (assert=f64 exp val)
   `(assert-predicate =f64 ,exp ,val))

(define-test-suite srfi74-tests

   (test "endianness works"
      (assert-equal? (endianness big) +big-endian+)
      (assert-equal? (endianness little) +little-endian+)
      (assert-equal? (endianness native) +native-endian+))
   
   (test "blob? works"
      (assert-true (blob? (make-blob 2)))
      (assert-false (blob? (vector))))

   (test "blob-length works"
      (assert= (blob-length (make-blob 3)) 3)
      (assert= (blob-length (make-blob 128)) 128))

   (test "blob works"
      (let ((t (blob #xff #x0 #x5 #x6)))
         (assert= (blob-length t) 4)
         (assert= (blob-u8-ref t 0) #xff)
         (assert= (blob-u8-ref t 1) 0)
         (assert= (blob-u8-ref t 2) #x5)
         (assert= (blob-u8-ref t 3) #x6)))

   (test "blob=? works"
      (let ((t1 (blob #xff #x01))
            (t2 (blob #xff #x01))
            (t3 (blob #x00 #xff)))
         (assert-true (blob=? t1 t2))
         (assert-false (blob=? t2 t3))))

   (test "blob-copy works"
      (let* ((t1 (make-random-blob 5))
             (t2 (blob-copy t1)))
         (assert-true (blob=? t1 t2))
         (assert-false (eq? t1 t2))))

   (test "blob-copy! works"
      (let ((t1 (make-random-blob 6))
            (t2 (make-blob 6)))
         (blob-copy! t1 0 t2 0 6)
         (assert-true (blob=? t1 t2))
         (blob-copy! t1 2 t2 0 2)
         (assert= (blob-u8-ref t1 2)
            (blob-u8-ref t2 0))
         (assert= (blob-u8-ref t1 3)
            (blob-u8-ref t2 1))
         (assert-exception-thrown (blob-copy! t1 0 t2 0 9) &error)))

   (test "blob-s8-ref works"
      (let ((t (blob #xf0 #x04)))
         (assert= (blob-s8-ref t 1) 4)
         (assert= (blob-s8-ref t 0) -16)))

   (test "blob-u8-ref works"
      (let ((t (blob #xf0 #x04)))
         (assert= (blob-u8-ref t 1) 4)
         (assert= (blob-u8-ref t 0) 240)))

   (test "blob-s8-set! works"
      (let ((t (make-blob 2)))
         (blob-s8-set! t 0 -1)
         (assert= (blob-u8-ref t 0) 255)
         (assert= (blob-s8-ref t 0) -1)
         (blob-s8-set! t 1 64)
         (assert= (blob-u8-ref t 1) 64)
         (assert= (blob-s8-ref t 1) 64)))

   (test "blob-u8-set! works"
      (let ((t (make-blob 2)))
         (blob-u8-set! t 0 255)
         (assert= (blob-u8-ref t 0) 255)
         (assert= (blob-s8-ref t 0) -1)
         (blob-u8-set! t 1 64)
         (assert= (blob-u8-ref t 1) 64)
         (assert= (blob-s8-ref t 1) 64)))

   (test "blob-s16-ref works"
      (let ((t (blob #xf0 #x04)))
         (assert= (blob-s16-ref +big-endian+ t 0) -4092)
         (assert= (blob-s16-ref +little-endian+ t 0) 1264)))

   (test "blob-u16-ref works"
      (let ((t (blob #xf0 #x04)))
         (assert= (blob-u16-ref +little-endian+ t 0) 1264)
         (assert= (blob-u16-ref +big-endian+ t 0) 61444)))

   (test "blob-s16-set! works"
      (let ((t (make-blob 2)))
         (blob-s16-set! +big-endian+ t 0 -1)
         (assert= (blob-u16-ref +little-endian+ t 0) 65535)
         (assert= (blob-s16-ref +little-endian+ t 0) -1)
         (blob-s16-set! +little-endian+ t 0 64)
         (assert= (blob-u16-ref +big-endian+ t 0) 16384)
         (assert= (blob-s16-ref +big-endian+ t 0) 16384)
         (assert-exception-thrown (blob-s16-set! +big-endian+ t 0 65536) &error)))

   (test "blob-u16-set! works"
      (let ((t (make-blob 2)))
         (blob-u16-set! +little-endian+ t 0 255)
         (assert= (blob-u16-ref +big-endian+ t 0) 65280)
         (assert= (blob-s16-ref +big-endian+ t 0) -256)
         (blob-u16-set! +big-endian+ t 0 255)
         (assert= (blob-u16-ref +little-endian+ t 0) 65280)
         (assert= (blob-s16-ref +little-endian+ t 0) -256)
         (assert-exception-thrown (blob-u16-set! +big-endian+ t 0 -1) &error)))

   (test "blob-s16-native-ref works"
      (let ((t (blob #xf0 #x04)))
         (if (eq? +native-endian+ +big-endian+)
             (assert= (blob-s16-native-ref t 0) -4092)
             (assert= (blob-s16-native-ref t 0) 1264))))
   
   (test "blob-u16-native-ref works"
      (let ((t (blob #xf0 #x04)))
         (if (eq? +native-endian+ +big-endian+)
             (assert= (blob-u16-native-ref t 0) 61444)
             (assert= (blob-u16-native-ref t 0) 1264))))
   
   (test "blob-s16-native-set! works"
      (let ((t (make-blob 2)))
         (blob-s16-native-set! t 0 -16)
         (assert= (blob-s16-native-ref t 0) -16)
         (assert= (blob-u16-native-ref t 0) 65520)
         (assert-exception-thrown (blob-s16-native-set! t 0 32768) &error)))
   
   (test "blob-u16-native-set! works"
      (let ((t (make-blob 2)))
         (blob-u16-native-set! t 0 65520)
         (assert= (blob-s16-native-ref t 0) -16)
         (assert= (blob-u16-native-ref t 0) 65520)
         (assert-exception-thrown (blob-u16-native-set! t 0 65536) &error)))

   (test "blob-s32-ref works"
      (let ((t (blob #xff #x21 #x05 #x00)))
         (assert= (blob-s32-ref +big-endian+ t 0) -14613248)
         (assert= (blob-s32-ref +little-endian+ t 0) 336383)))
   
   (test "blob-u32-ref works"
      (let ((t (blob #xf0 #x04 #x00 #x23)))
         (assert= (blob-u32-ref +little-endian+ t 0) 587203824)
         (assert= (blob-u32-ref +big-endian+ t 0) #l4026794019)))
   
   (test "blob-s32-set! works"
      (let ((t (make-blob 4)))
         (blob-s32-set! +big-endian+ t 0 -1)
         (assert= (blob-u32-ref +little-endian+ t 0) #l4294967295)
         (assert= (blob-s32-ref +little-endian+ t 0) -1)
         (blob-s32-set! +little-endian+ t 0 64)
         (assert= (blob-u32-ref +big-endian+ t 0) 1073741824)
         (assert= (blob-s32-ref +big-endian+ t 0) 1073741824)
         (assert-exception-thrown (blob-s32-set! +big-endian+ t 0 #l4294967296) &error)))

   (test "blob-u32-set! works"
      (let ((t (make-blob 4)))
         (blob-u32-set! +little-endian+ t 0 255)
         (assert= (blob-u32-ref +big-endian+ t 0) #l4278190080)
         (assert= (blob-s32-ref +big-endian+ t 0) #l-16777216)
         (blob-u32-set! +big-endian+ t 0 255)
         (assert= (blob-u32-ref +little-endian+ t 0) #l4278190080)
         (assert= (blob-s32-ref +little-endian+ t 0) #l-16777216)
         (assert-exception-thrown (blob-u32-set! +big-endian+ t 0 -1) &error)))

   (test "blob-s32-native-ref works"
      (let ((t (blob #xf0 #x04 #x42 #x03)))
         (if (eq? +native-endian+ +big-endian+)
             (assert= (blob-s32-native-ref t 0) #l-268156413)
             (assert= (blob-s32-native-ref t 0) #l54658288))))

   (test "blob-u32-native-ref works"
      (let ((t (blob #xf0 #x04 #x42 #x03)))
         (if (eq? +native-endian+ +big-endian+)
             (assert= (blob-u32-native-ref t 0)  #l4026810883)
             (assert= (blob-u32-native-ref t 0) #l54658288))))


   (test "blob-s32-native-set! works"
      (let ((t (make-blob 4)))
         (blob-s32-native-set! t 0 -16)
         (assert= (blob-s32-native-ref t 0) -16)
         (assert= (blob-u32-native-ref t 0) #l4294967280)
         (assert-exception-thrown (blob-s32-native-set! t 0 #l4294967296) &error)))

   (test "blob-u32-native-set! works"
      (let ((t (make-blob 4)))
         (blob-u32-native-set! t 0 #l2147483648)
         (assert= (blob-s32-native-ref t 0) #l-2147483648)
         (assert= (blob-u32-native-ref t 0) #l2147483648)
         (assert-exception-thrown (blob-u32-native-set! t 0 #z1099511627776) &error)))

   (test "blob-s64-ref works"
      (let ((t (blob #xff #x21 #x05 #x00 #xff #x21 #x05 #x00)))
         (assert= (blob-s64-ref +big-endian+ t 0) #z-62763417967983360)
         (assert= (blob-s64-ref +little-endian+ t 0) #z1444753984266751)))
   
   (test "blob-u64-ref works"
      (let ((t (blob #xf0 #x04 #x00 #x23 #xf0 #x04 #x00 #x23)))
         (assert= (blob-u64-ref +little-endian+ t 0)  #z2522021220753343728)
         (assert= (blob-u64-ref +big-endian+ t 0) #z17294948623360196643)))
   
   (test "blob-s64-set! works"
      (let ((t (make-blob 8)))
         (blob-s64-set! +big-endian+ t 0 -1)
         (assert= (blob-u64-ref +little-endian+ t 0) #z18446744073709551615)
         (assert= (blob-s64-ref +little-endian+ t 0) -1)
         (blob-s64-set! +little-endian+ t 0 64)
         (assert= (blob-u64-ref +big-endian+ t 0) #z4611686018427387904)
         (assert= (blob-s64-ref +big-endian+ t 0) #z4611686018427387904)
         (assert-exception-thrown (blob-s64-set! +big-endian+ t 0 #z18446744073709551616) &error)
         ))

   (test "blob-u64-set! works"
      (let ((t (make-blob 8)))
         (blob-u64-set! +little-endian+ t 0 255)
         (assert= (blob-u64-ref +big-endian+ t 0) #z18374686479671623680)
         (assert= (blob-s64-ref +big-endian+ t 0) #z-72057594037927936)
         (blob-u64-set! +big-endian+ t 0 255)
         (assert= (blob-u64-ref +little-endian+ t 0) #z18374686479671623680)
         (assert= (blob-s64-ref +little-endian+ t 0) #z-72057594037927936)
         (assert-exception-thrown (blob-u64-set! +big-endian+ t 0 -1) &error)))
   
   (test "blob-s64-native-ref works"
      (let ((t (blob #xf0 #x04 #x42 #x03 #xf0 #x04 #x42 #x03)))
         (if (eq? +native-endian+ +big-endian+)
             (assert= (blob-s64-native-ref t 0) #z-1151723020020858365)
             (assert= (blob-s64-native-ref t 0) #z234755559470007536))))

   (test "blob-u64-native-ref works"
      (let ((t (blob #xf0 #x04 #x42 #x03 #xf0 #x04 #x42 #x03)))
         (if (eq? +native-endian+ +big-endian+)
             (assert= (blob-u64-native-ref t 0) #z17295021053688693251)
             (assert= (blob-u64-native-ref t 0) #z234755559470007536))))

   (test "blob-s64-native-set! works"
      (let ((t (make-blob 8)))
         (blob-s64-native-set! t 0 -16)
         (assert= (blob-s64-native-ref t 0) -16)
         (assert= (blob-u64-native-ref t 0) #z18446744073709551600)
         (assert-exception-thrown (blob-s64-native-set! t 0 #z9223372036854775808) &error)))

   (test "blob-u64-native-set! works"
      (let ((t (make-blob 8)))
         (blob-u64-native-set! t 0 #z18446744073709551615)
         (assert= (blob-s64-native-ref t 0) -1)
         (assert= (blob-u64-native-ref t 0) #z18446744073709551615)
         (assert-exception-thrown (blob-u64-native-set! t 0 #z18446744073709551616) &error)))

   (test "blob-uint-ref works"
      (do ((i 0 (+ i 1)))
          ((= i 100))
          (let ((blob16 (make-random-blob 2))
                (blob32 (make-random-blob 4))
                (blob64 (make-random-blob 8)))
             (assert= (blob-uint-ref 2 +little-endian+ blob16 0)
                (blob-u16-ref +little-endian+ blob16 0))
             (assert= (blob-uint-ref 4 +little-endian+ blob32 0)
                (blob-u32-ref +little-endian+ blob32 0))
             (assert= (blob-uint-ref 8 +little-endian+ blob64 0)
                (blob-u64-ref +little-endian+ blob64 0))
             (assert= (blob-uint-ref 2 +big-endian+ blob16 0)
                (blob-u16-ref +big-endian+ blob16 0))
             (assert= (blob-uint-ref 4 +big-endian+ blob32 0)
                (blob-u32-ref +big-endian+ blob32 0))
             (assert= (blob-uint-ref 8 +big-endian+ blob64 0)
                (blob-u64-ref +big-endian+ blob64 0))
             )))

   (test "blob-sint-ref works"
      (do ((i 0 (+ i 1)))
          ((= i 100))
          (let ((blob16 (make-random-blob 2))
                (blob32 (make-random-blob 4))
                (blob64 (make-random-blob 8)))
             (assert= (blob-sint-ref 2 +little-endian+ blob16 0)
                (blob-s16-ref +little-endian+ blob16 0))
             (assert= (blob-sint-ref 4 +little-endian+ blob32 0)
                (blob-s32-ref +little-endian+ blob32 0))
             (assert= (blob-sint-ref 8 +little-endian+ blob64 0)
                (blob-s64-ref +little-endian+ blob64 0))
             (assert= (blob-sint-ref 2 +big-endian+ blob16 0)
                (blob-s16-ref +big-endian+ blob16 0))
             (assert= (blob-sint-ref 4 +big-endian+ blob32 0)
                (blob-s32-ref +big-endian+ blob32 0))
             (assert= (blob-sint-ref 8 +big-endian+ blob64 0)
                (blob-s64-ref +big-endian+ blob64 0)))))

   (test "blob-sint-set! works"
      (do ((i 0 (+ i 1)))
          ((= i 100))
          (let ((blob16 (make-blob 2))
                (blob32 (make-blob 4))
                (blob64 (make-blob 8))
                (test16 (make-random-signed-int 2))
                (test32 (make-random-signed-int 4))
                (test64 (make-random-signed-int 8)))
             ;; 16bits
             (blob-sint-set! 2 +little-endian+ blob16 0 test16)
             (assert= (blob-sint-ref 2 +little-endian+ blob16 0)
                test16)
             (assert= (blob-s16-ref +little-endian+ blob16 0)
                test16)
             (blob-sint-set! 2 +big-endian+ blob16 0 test16)
             (assert= (blob-sint-ref 2 +big-endian+ blob16 0)
                test16)
             (assert= (blob-s16-ref +big-endian+ blob16 0)
                test16)
             ;; 32 bits
             (blob-sint-set! 4 +little-endian+ blob32 0 test32)
             (assert= (blob-sint-ref 4 +little-endian+ blob32 0)
                test32)
             (assert= (blob-s32-ref +little-endian+ blob32 0)
                test32)
             (blob-sint-set! 4 +big-endian+ blob32 0 test32)
             (assert= (blob-sint-ref 4 +big-endian+ blob32 0)
                test32)
             (assert= (blob-s32-ref +big-endian+ blob32 0)
                test32)
             ;; 64 bits
             (blob-sint-set! 8 +little-endian+ blob64 0 test64)
             (assert= (blob-sint-ref 8 +little-endian+ blob64 0)
                test64)
             (assert= (blob-s64-ref +little-endian+ blob64 0)
                test64)
             (blob-sint-set! 8 +big-endian+ blob64 0 test64)
             (assert= (blob-sint-ref 8 +big-endian+ blob64 0)
                test64)
             (assert= (blob-s64-ref +big-endian+ blob64 0)
                test64))))

   (test "blob-uint-set! works"
      (do ((i 0 (+ i 1)))
          ((= i 100))
          (let ((blob16 (make-blob 2))
                (blob32 (make-blob 4))
                (blob64 (make-blob 8))
                (test16 (make-random-unsigned-int 2))
                (test32 (make-random-unsigned-int 4))
                (test64 (make-random-unsigned-int 8)))
             ;; 16bits
             (blob-uint-set! 2 +little-endian+ blob16 0 test16)
             (assert= (blob-uint-ref 2 +little-endian+ blob16 0)
                test16)
             (assert= (blob-u16-ref +little-endian+ blob16 0)
                test16)
             (blob-uint-set! 2 +big-endian+ blob16 0 test16)
             (assert= (blob-uint-ref 2 +big-endian+ blob16 0)
                test16)
             (assert= (blob-u16-ref +big-endian+ blob16 0)
                test16)
             ;; 32 bits
             (blob-uint-set! 4 +little-endian+ blob32 0 test32)
             (assert= (blob-uint-ref 4 +little-endian+ blob32 0)
                test32)
             (assert= (blob-u32-ref +little-endian+ blob32 0)
                test32)
             (blob-uint-set! 4 +big-endian+ blob32 0 test32)
             (assert= (blob-uint-ref 4 +big-endian+ blob32 0)
                test32)
             (assert= (blob-u32-ref +big-endian+ blob32 0)
                test32)
             ;; 64 bits
             (blob-uint-set! 8 +little-endian+ blob64 0 test64)
             (assert= (blob-uint-ref 8 +little-endian+ blob64 0)
                test64)
             (assert= (blob-u64-ref +little-endian+ blob64 0)
                test64)
             (blob-uint-set! 8 +big-endian+ blob64 0 test64)
             (assert= (blob-uint-ref 8 +big-endian+ blob64 0)
                test64)
             (assert= (blob-u64-ref +big-endian+ blob64 0)
                test64))))


   (test "blob->u8-list works"
      (let* ((b (make-random-blob 4))
             (l (blob->u8-list b)))
         (assert= (blob-u8-ref b 0)
            (list-ref l 0))
         (assert= (blob-u8-ref b 1)
            (list-ref l 1))
         (assert= (blob-u8-ref b 2)
            (list-ref l 2))
         (assert= (blob-u8-ref b 3)
            (list-ref l 3))))

   (test "u8-list->blob works"
      (let* ((l (make-random-u8-list 4))
             (b (u8-list->blob l)))
         (assert= (list-ref l 0)
            (blob-u8-ref b 0))
         (assert= (list-ref l 1)
            (blob-u8-ref b 1))
         (assert= (list-ref l 2)
            (blob-u8-ref b 2))
         (assert= (list-ref l 3)
            (blob-u8-ref b 3))
         (assert-exception-thrown (u8-list->blob '(32 44 300)) &error)))

   (test "blob->uint-list works"
      (let* ((b (make-random-blob 16))
             (l16 (blob->uint-list 2 +little-endian+ b))
             (l32 (blob->uint-list 4 +big-endian+ b))
             (l64 (blob->uint-list 8 +little-endian+ b))
             (l128 (blob->uint-list 16 +big-endian+ b)))
         (assert= (list-ref l16 0)
            (blob-uint-ref 2 +little-endian+ b 0))
         (assert= (list-ref l16 3)
            (blob-uint-ref 2 +little-endian+ b 6))
         (assert= (list-ref l32 0)
            (blob-uint-ref 4 +big-endian+ b 0))
         (assert= (list-ref l32 3)
            (blob-uint-ref 4 +big-endian+ b 12))
         (assert= (list-ref l64 0)
            (blob-uint-ref 8 +little-endian+ b 0))
         (assert= (list-ref l64 1)
            (blob-uint-ref 8 +little-endian+ b 8))
         (assert= (list-ref l128 0)
            (blob-uint-ref 16 +big-endian+ b 0))
         (assert-exception-thrown (blob->uint-list 32 +little-endian+ b) &error)
         ))

   (test "blob->sint-list works"
      (let* ((b (make-random-blob 16))
             (l16 (blob->sint-list 2 +little-endian+ b))
             (l32 (blob->sint-list 4 +big-endian+ b))
             (l64 (blob->sint-list 8 +little-endian+ b))
             (l128 (blob->sint-list 16 +big-endian+ b)))
         (assert= (list-ref l16 0)
            (blob-sint-ref 2 +little-endian+ b 0))
         (assert= (list-ref l16 3)
            (blob-sint-ref 2 +little-endian+ b 6))
         (assert= (list-ref l32 0)
            (blob-sint-ref 4 +big-endian+ b 0))
         (assert= (list-ref l32 3)
            (blob-sint-ref 4 +big-endian+ b 12))
         (assert= (list-ref l64 0)
            (blob-sint-ref 8 +little-endian+ b 0))
         (assert= (list-ref l64 1)
            (blob-sint-ref 8 +little-endian+ b 8))
         (assert= (list-ref l128 0)
            (blob-sint-ref 16 +big-endian+ b 0))
         (assert-exception-thrown (blob->sint-list 32 +little-endian+ b) &error)))

   (test "uint-list->blob works"
      (let ((u16list (make-random-uint-list 2 16))
            (u32list (make-random-uint-list 4 32))
            (u64list (make-random-uint-list 8 64))
            (u128list (make-random-uint-list 16 128)))
         (assert-equal?   (blob->uint-list 2 +little-endian+ (uint-list->blob 2 +little-endian+ u16list)) u16list)
         (assert-equal?   (blob->uint-list 4 +little-endian+ (uint-list->blob 4 +little-endian+ u32list)) u32list)
         (assert-equal?   (blob->uint-list 8 +little-endian+ (uint-list->blob 8 +little-endian+ u64list)) u64list)
         (assert-equal?   (blob->uint-list 16 +little-endian+ (uint-list->blob 16 +little-endian+ u128list)) u128list)))

   (test "sint-list->blob works"
      (let ((s16list (make-random-sint-list 2 16))
            (s32list (make-random-sint-list 4 32))
            (s64list (make-random-sint-list 8 64))
            (s128list (make-random-sint-list 16 128)))
         (assert-equal?   (blob->sint-list 2 +little-endian+ (sint-list->blob 2 +little-endian+ s16list)) s16list)
         (assert-equal?   (blob->sint-list 4 +little-endian+ (sint-list->blob 4 +little-endian+ s32list)) s32list)
         (assert-equal?   (blob->sint-list 8 +little-endian+ (sint-list->blob 8 +little-endian+ s64list)) s64list)
         (assert-equal?   (blob->sint-list 16 +little-endian+ (sint-list->blob 16 +little-endian+ s128list)) s128list)))


   (test "blob-f32-ref works"
      (let ((b1 (blob #x46 #x01 #x2e #x8a))
            (b2 (blob #x12 #x34 #x56 #x78)))
         (assert=f32 (blob-f32-ref b1 0)
            8267.634765625)
         (assert=f32  (blob-f32-ref b2 0) 5.6904566139035e-28)))


   (test "blob-f64-ref works"
      (let ((b1 (blob #x40 #xab #x5a #x75 #x31 #xf1 #xb4 #xea))
            (b2 (blob #x40 #xee #x64 #xbd #xb4 #x92 #x01 #x7b)))
         (assert=f64 (blob-f64-ref b1 0) 3501.228896668753)
         (assert=f64 (blob-f64-ref b2 0) 62245.92829227723)))

   (test "blob-f32-set! works"
      (do ((i 0 (+ i 1)))
          ((= i 100))
          (let* ((test-real (make-random-real 10000))
                 (b (make-blob 4)))
             (blob-f32-set! b 0 test-real)
             (assert=f32 (blob-f32-ref b 0) test-real))))

   (test "blob-f64-set! works"
      (do ((i 0 (+ i 1)))
          ((= i 100))
          (let* ((test-real (make-random-real 10000))
                 (b (make-blob 8)))
             (blob-f64-set! b 0 test-real)
             (assert=f64 (blob-f64-ref b 0) test-real))))


   (test "blob->f32-list works"
      (let* ((b (make-random-blob 32))
             (lf32 (blob->f32-list b)))
         (do ((i 0 (+ i 1)))
             ((= i 8))
             (assert=f32 (blob-f32-ref b (* i 4))
                (list-ref lf32 i)))))

   (test "f32-list->blob works"
      (let* ((lf32 (make-random-real-list 10000 8))
             (b (f32-list->blob lf32)))
         (for-each (lambda (a b)
                      (assert=f32 a b))
              (blob->f32-list b) lf32)))

   (test "blob->f64-list works"
      (let* ((b (make-random-blob 64))
             (lf64 (blob->f64-list b)))
         (do ((i 0 (+ i 1)))
             ((= i 8))
             (assert=f64 (blob-f64-ref b (* i 8))
                (list-ref lf64 i)))))


   (test "f64-list->blob works"
      (let* ((lf64 (make-random-real-list 10000 8))
             (b (f64-list->blob lf64)))
         (for-each (lambda (a b)
                      (assert=f64 a b))
            (blob->f64-list b) lf64)))

   
   (test "string->blob works"
      (let ((b (string->blob (apply string (map integer->char '(#xff #x21 #x05 #x00))))))
         (assert= (blob-s32-ref +big-endian+ b 0) -14613248)
         (assert= (blob-s32-ref +little-endian+ b 0) 336383)))

   (test "string->blob! works"
      (let* ((str (apply string (map integer->char '(#xff #x21 #x05 #x00))))
             (b (string->blob! str)))
         (assert= (blob-s32-ref +big-endian+ b 0) -14613248)
         (assert= (blob-s32-ref +little-endian+ b 0) 336383)
         (string-set! str 3 (integer->char #x01))
         (assert= (blob-s32-ref +big-endian+ b 0) -14613247 )
         (assert= (blob-s32-ref +little-endian+ b 0) 17113599)))

   (test "blob->string works"
      (do ((i 0 (+ i 1)))
          ((= i 100))
          (let* ((b (make-random-blob 8))
                 (b2 (string->blob (blob->string b))))
             (assert-equal? b2  b)
             (assert-false (eq? b2 b)))))

   (test "equal? works with blob"
      (let ((b (blob #xff #x21 #x05 #x00))
            (b2 (blob #xff #x21 #x05 #x00)))
         (assert-equal? b b2)
         (assert-false (equal? b '(#xff #x21 #x05 #x00)))))

   (test "blob-str-ref works"

      (let ((b (string->blob "test string randome stuff")))
         (assert-equal? (blob-str-ref b 5 6) "string")
         (assert-equal? (blob-str-ref b 20 5) "stuff")
         (assert-exception-thrown (blob-str-ref b 20 6) &error)))

   (test "blob-str-set! works"
      (let ((b (make-blob 10)))
         (blob-str-set! b 4 "yay!")
         (assert-equal? (blob-str-ref b 4 4) "yay!")
         (blob-str-set! b 1 "dogs")
         (assert-equal? (blob-str-ref b 1 7)
            "dogsay!")))

   (test "blob-cstr-ref works"
      (let ((b1 (blob 100 111 103 110 97 98 98 105 116 0))
            (b2 (blob 100 111 103 110 97 98 98 105 116)))
         (assert-equal? (blob-cstr-ref b1 3) "nabbit")
         (assert-exception-thrown (blob-cstr-ref b2 0) &error)))


   (test "blob-bit-ref works"
      (let ((b (blob #x3 #xf0)))
         (assert= (blob-bit-ref b 0 6) 1)
         (assert= (blob-bit-ref b 0 1) 0)
         (assert= (blob-bit-ref b 1 7) 0)
         (assert= (blob-bit-ref b 1 0) 1)
         (assert-exception-thrown (blob-bit-ref b 1 8) &error)))

   (test "blob-bits-set! works"
      (let ((b (make-blob 4))
            (b2 (make-blob 1)))
         (blob-bits-set! b 0 3 5 #b101)
         (assert= (blob-bit-ref b 0 7) 1)
         (assert= (blob-bit-ref b 0 6) 0)
         (assert= (blob-bit-ref b 0 5) 1)
         (assert= (blob-bit-ref b 0 4) 0)
         (assert= (blob-bit-ref b 0 3) 0)
         (assert= (blob-bit-ref b 0 2) 0)
         (assert= (blob-bit-ref b 0 1) 0)
         (assert= (blob-bit-ref b 0 0) 0)
         (blob-bits-set! b2 0 0 2 1) 
         (assert= (blob-bit-ref b2 0 0) 0)
         (assert= (blob-bit-ref b2 0 1) 1)
         (assert-exception-thrown (blob-bits-set! b 0 0 65 #xFFFFFFFFFFFFFF)
            &error)))

   (test "blob-grow! works"
      (let ((b1 (make-blob 1))
            (b2 (string->blob "test")))
         (assert= (blob-length b1) 1)
         (blob-grow! b1 4)
         (assert= (blob-length b1) 4)
         (assert-exception-thrown (blob-grow! b1 3) &error)
         (assert= (blob-length b2) 4)
         (blob-grow! b2 10)
         (assert= (blob-length b2) 10)))


   (test "blob-shrink! works"
      (let ((b1 (make-blob 4))
            (b2 (string->blob "test")))
         (assert= (blob-length b1) 4)
         (blob-shrink! b1 1)
         (assert= (blob-length b1) 1)
         (assert-exception-thrown (blob-shrink! b1 3) &error)
         (assert= (blob-length b2) 4)
         (blob-shrink! b2 2)
         (assert= (blob-length b2) 2)
         (assert-equal? (blob-u8-ref b2 1) (char->integer #\e))))

   

   ;;;; some adapted test from reference implementation

   (test "example tests"
      (define b1 (make-blob 16))
      (assert= (blob-length b1) 16)

      (blob-u8-set! b1 0 223)
      (blob-s8-set! b1 1 123)
      (blob-s8-set! b1 2 -123)
      (blob-u8-set! b1 3 15)
      
      (assert-equal? (list (blob-u8-ref b1 0)
                        (blob-s8-ref b1 1)
                        (blob-u8-ref b1 1)
                        (blob-s8-ref b1 2)
                        (blob-u8-ref b1 2)
                        (blob-u8-ref b1 3))
         '(223 123 123 -123 133 15))

      (blob-uint-set! 16 (endianness little)
         b1 0 (- (exptbx #z2 #z128) 3))

      (assert= (blob-uint-ref 16 (endianness little) b1 0)
          (- (exptbx #z2 #z128) 3))

      (assert= (blob-sint-ref 16 (endianness little) b1 0)
         -3)

      (assert-equal? (blob->u8-list (u8-list->blob '(253 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255))) '(253 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255))
      
      (assert-equal? (blob->u8-list b1)
         '(253 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255))

      (blob-uint-set! 16 (endianness big)
         b1 0 (- (expt 2 128) 3))

      (assert= (blob-uint-ref 16 (endianness big) b1 0)
         (- (exptbx #z2 #z128) 3))

      (assert= (blob-sint-ref 16 (endianness big) b1 0)
         -3)

      (assert-equal? (blob->u8-list b1)
         '(255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 253)
         )

      (assert= (blob-u16-ref (endianness little) b1 14)
         65023)
      (assert= (blob-s16-ref (endianness little) b1 14)
         -513)
      (assert= (blob-u16-ref (endianness big) b1 14)
         65533)
      (assert= (blob-s16-ref (endianness big) b1 14)
         -3)

      (blob-u16-set! (endianness little) b1 0 12345)
      
      (blob-u16-native-set! b1 0 12345)

      (assert= (blob-u16-native-ref b1 0)
         12345)

      (assert= (blob-u32-ref (endianness little) b1 12)
        #l4261412863)
      (assert= (blob-s32-ref (endianness little) b1 12)
         -33554433)
      (assert= (blob-u32-ref (endianness big) b1 12)
          #l4294967293)
      
      (assert= (blob-s32-ref (endianness big) b1 12)
         -3)

      (blob-u32-set! (endianness little) b1 0 12345)
      
      (blob-u32-native-set! b1 0 12345)
      
      (assert= (blob-u32-native-ref b1 0)
         12345)

      (assert= (blob-u64-ref (endianness little) b1 8)
         #z18302628885633695743)
      (assert= (blob-s64-ref (endianness little) b1 8)
         #z-144115188075855873)
      (assert= (blob-u64-ref (endianness big) b1 8)
         #z18446744073709551613)
      (assert= (blob-s64-ref (endianness big) b1 8)
         -3)

      (blob-u64-set! (endianness little) b1 0 12345)
      
      (blob-u64-native-set! b1 0 12345)

      (assert= (blob-u64-native-ref b1 0)
         12345)
      
      (define b2 (u8-list->blob '(1 2 3 4 5 6 7 8)))
      (define b3 (blob-copy b2))

      (assert-true (blob=? b2 b3))
      (assert-false (blob=? b1 b2))

      (blob-copy! b3 0 b3 4 4)
      
      (assert-equal? (blob->u8-list b3)
         '(1 2 3 4 1 2 3 4))

      ;;          s  ss t ts l
      (blob-copy! b3 0 b3 2 6)
      
      (assert-equal? (blob->u8-list b3)
         '(1 2 1 2 3 4 1 2))

      (blob-copy! b3 2 b3 0 6)
      
      (assert-equal? (blob->u8-list b3) '(1 2 3 4 1 2 1 2))

      (assert-equal? (blob->uint-list 1 (endianness little) b3)
         '(1 2 3 4 1 2 1 2))

      (assert-equal? (blob->uint-list 2 (endianness little) b3)
         '(513 1027 513 513))

      (define b4 (u8-list->blob '(0 0 0 0 0 0 48 57 255 255 255 255 255 255 255 253)))
      
      (assert-equal? (blob->sint-list 2 (endianness little) b4)
         '(0 0 0 14640 -1 -1 -1 -513))
      )

   
   )


(define (main args)
   (let ((tr (instantiate::terminal-test-runner (suite srfi74-tests))))
      (if (test-runner-execute tr #t) 0 -1)))