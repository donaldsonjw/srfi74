(module blob
   (include "blob.sch")
   (export
      (class %blob
         ;;u8vector, string, or mmap
         data)
      +big-endian+::symbol
      +little-endian+::symbol
      +native-endian+::symbol
      (blob?::bool obj)
      (make-blob::%blob size::long)
      (blob::%blob #!rest bytes)
      (blob-copy::%blob blob::%blob)
      (blob-copy! source::%blob source-start::long target::%blob
           target-start::long n::long)
      (blob=?::bool blob1::%blob blob2::%blob)
      (blob-length::long blob::%blob)
      (blob-u8-ref::bint blob::%blob index::long)
      (blob-s8-ref::bint blob::%blob index::long)
      (blob-u8-set! blob::%blob index::long val::uint8)
      (blob-s8-set! blob::%blob index::long val::int8)
      (blob-u16-ref endianness::symbol blob::%blob index::long)
      (blob-s16-ref endianness::symbol blob::%blob index::long)
      (blob-s16-set! endianness::symbol blob::%blob index::long val)
      (blob-u16-set! endianness::symbol blob::%blob index::long val)
      (blob-u16-native-ref blob::%blob index::long)
      (blob-s16-native-ref blob::%blob index::long)
      (blob-u16-native-set! blob::%blob index::long val)
      (blob-s16-native-set! blob::%blob index::long val)
      (blob-u32-ref endianness::symbol blob::%blob index::long)
      (blob-s32-ref endianness::symbol blob::%blob index::long)
      (blob-u32-native-ref blob::%blob index::long)
      (blob-s32-native-ref blob::%blob index::long)
      (blob-u32-set! endianness::symbol blob::%blob index::long val)
      (blob-s32-set! endianness::symbol blob::%blob index::long val)
      (blob-u32-native-set! blob::%blob index::long val)
      (blob-s32-native-set! blob::%blob index::long val)
      (blob-u64-ref endianness::symbol blob::%blob index::long)
      (blob-s64-ref endianness::symbol blob::%blob index::long)
      (blob-u64-native-ref blob::%blob index::long)
      (blob-s64-native-ref blob::%blob index::long)
      (blob-u64-set! endianness::symbol blob::%blob index::long val)
      (blob-s64-set! endianness::symbol blob::%blob index::long val)
      (blob-u64-native-set! blob::%blob index::long val)
      (blob-s64-native-set! blob::%blob index::long val)
      (blob-uint-ref size endianness::symbol blob::%blob index::long)
      (blob-sint-ref size endianness::symbol blob::%blob index::long)
      (blob-uint-set! size endianness::symbol blob::%blob index::long val)
      (blob-sint-set! size endianness::symbol blob::%blob index::long val)
      (blob->u8-list::pair-nil blob::%blob)
      (u8-list->blob::%blob lst::pair-nil)
      (blob->uint-list::pair-nil size::long endianness::symbol blob::%blob)
      (blob->sint-list::pair-nil size::long endianness::symbol blob::%blob)
      (uint-list->blob::%blob size::long endianness::symbol lst::pair-nil)
      (sint-list->blob::%blob size::long endianness::symbol lst::pair-nil)
      (blob-f32-set! blob::%blob index::long val::float)
      (blob-f32-ref blob::%blob index::long)
      (blob-f64-set! blob::%blob index::long val::double)
      (blob-f64-ref blob index::long)
      (blob->f32-list::pair-nil blob::%blob)
      (f32-list->blob::%blob list::pair-nil)
      (blob->f64-list::pair-nil blob::%blob)
      (f64-list->blob::%blob lst::pair-nil)
      (string->blob::%blob str::bstring)
      (string->blob!::%blob str::bstring)
      (blob->string blob::%blob)
      (u8vector->blob::%blob vec::u8vector)
      (mmap->blob::%blob map::mmap)
      (blob-str-ref blob::%blob index::long length::long)
      (blob-str-set! blob::%blob index::long str::bstring)
      (blob-cstr-ref blob::%blob index::long)))
   

(define (pow256 x)
   (cond ((= x 0)
          1)
         ((= x 1)
          256)
         ((even? x)
          (let ((t (pow256 (quotient x 2))))
             (* t t)))
         (else
          (* 256 (pow256 (- x 1))))))

(define-macro (->uint8 val)
   (let ((v1 (gensym 'val)))
      `(let ((,v1 ,val))
          (cond ((fixnum? ,v1)
                 (fixnum->uint8 ,v1))
                ((elong? ,v1)
                 (fixnum->uint8 (elong->fixnum ,v1)))
                ((llong? ,v1)
                 (fixnum->uint8 (llong->fixnum ,v1)))
                ((bignum? ,v1)
                 (fixnum->uint8 (bignum->fixnum ,v1)))
                ((char? ,v1)
                 (fixnum->uint8 (char->integer ,v1)))
                (else
                 (error "->uint8" "argument not an integer" ,val))))))

(define-macro (->llong val)
   (let ((v1 (gensym 'val)))
      `(let ((,v1 ,val))
          (cond ((fixnum? ,v1)
                 (fixnum->llong ,v1))
                ((elong? ,v1)
                 (elong->llong ,v1))
                ((llong? ,v1)
                 ,v1)
                ((bignum? ,v1)
                 (bignum->llong ,v1))
                (else
                 (error "->llong" "argument not an integer" ,val))))))

(define-macro (->fixnum val)
   (let ((v1 (gensym 'val)))
      `(let ((,v1 ,val))
          (cond ((fixnum? ,v1)
                 ,v1)
                ((elong? ,v1)
                 (elong->fixnum ,v1))
                ((llong? ,v1)
                 (llong->fixnum ,v1))
                ((bignum? ,v1)
                 (bignum->fixnum ,v1))
                ((uint8? ,v1)
                 (uint8->fixnum ,v1))
                ((int8? ,v1)
                 (int8->fixnum ,v1))
                (else
                 (error "->fixnum" "argument not an integer" ,val))))))


(define-inline (u8vector-copy vec::u8vector)
   (let ((res (make-u8vector (u8vector-length vec))))
      (u8vector-copy! res 0 vec)
      vec))

(define +big-endian+ 'big-endian)
(define +little-endian+ 'little-endian)
(define +native-endian+ (bigloo-config 'endianess))

(define (blob? obj)
   (isa? obj %blob))

(define (make-blob size)
   (instantiate::%blob (data (make-u8vector size 0))))

(define (blob-copy blob::%blob)
   (let ((data-copy (cond ((u8vector? (-> blob data))
                           (u8vector-copy (-> blob data)))
                          ((string? (-> blob data))
                           (string-copy (-> blob data)))
                          ((mmap? (-> blob data))
                           (string-copy (blob->string blob))))))
      (instantiate::%blob (data data-copy))))

; (define (blob-copy! source::%blob source-start::long target::%blob
;            target-start::long n::long)
;    (if (and (>= source-start 0)
;             (<= (+ source-start n) (blob-length source))
;             (>= target-start 0)
;             (<= (+ target-start n) (blob-length target)))
;        (cond  ((and (u8vector? (-> target data))
;                     (u8vector? (-> source data)))
;                (u8vector-copy! (-> target data) target-start (-> source data) source-start (+ source-start n)))
;               (else
;                (do ((si source-start (+ si 1))
;                     (ti target-start (+ ti 1))
;                     (i 0 (+ i 1)))
;                    ((= i n))
;                    (blob-u8-set! target ti (->uint8 (blob-u8-ref source si)))
;                    )))
       ; (error "blob-copy!" "invalid arguments" (format "blob-copy! ~a ~a ~a ~a ~a"
       ;                                            source source-start target
       ;                                            target-start n))))

(define (blob-copy! source::%blob source-start::long target::%blob
           target-start::long n::long)
   (if (and (>=fx source-start 0)
            (<=fx (+ source-start n) (blob-length source))
            (>=fx target-start 0)
            (<=fx (+ target-start n) (blob-length target)))
       (let ((overlapping? (eq? (-> source data)
                              (-> target data))))
          (cond   ((and overlapping?
                        (= source-start target-start))
                   ; nothing todo
                   #unspecified)
                  ((and overlapping?
                        (and (< source-start target-start)
                             (< target-start (+ source-start n))))
                   (do ((si::long (-fx (+fx source-start n) 1) (-fx si 1))
                        (ti::long  (-fx (+fx target-start n) 1) (-fx ti 1))
                        (i::long (-fx n 1) (-fx i 1)))
                       ((<fx i 0))
                       (blob-u8-set! target ti (->uint8 (blob-u8-ref source si)))))
                  (else
                   (do ((si::long source-start (+fx si 1))
                        (ti::long target-start (+fx ti 1))
                        (i::long 0 (+fx i 1)))
                       ((=fx i n))
                       (blob-u8-set! target ti (->uint8 (blob-u8-ref source si)))))))
       (error "blob-copy!" "invalid arguments" (format "blob-copy! ~a ~a ~a ~a ~a"
                                                  source source-start target
                                                  target-start n))))


(define (byte? val)
   (and (integer? val)
        (>= val 0)
        (< val 256)))

(define (blob #!rest bytes)
   (if (every byte? bytes)
       (let* ((len (length bytes))
             (res (make-blob len)))
          (do ((i::long 0 (+fx i 1))
               (bs bytes (cdr bs)))
              ((=fx i len) res)
              (blob-u8-set! res i  (->uint8 (car bs)))))
       (error "blob" "bytes must be a list of bytes" bytes)))

(define (blob-length::long blob::%blob)
   (cond ((u8vector? (-> blob data))
          (u8vector-length (-> blob data)))
         ((string? (-> blob data))
          (string-length (-> blob data)))
         ((mmap? (-> blob data))
          (mmap-length (-> blob data)))
         (else (error "blob-length" "unknown blob type" blob))))

(define (blob=? blob1::%blob blob2::%blob)
  
   (cond ((or (and (u8vector? (-> blob1 data))
                   (u8vector? (-> blob2 data)))
              (and (string? (-> blob1 data))
                   (string? (-> blob2 data))))
          (equal? (-> blob1 data)
             (-> blob2 data)))
         (else
          (and (=fx (blob-length blob1)
                  (blob-length blob2))
               (do ((i::long 0 (+fx i 1))
                    (res #t (=fx (blob-u8-ref blob1 i)
                               (blob-u8-ref blob2 i))))
                   ((or (not res)
                        (=fx i (blob-length blob1))) res))))))

(define (blob-u8-ref::bint blob::%blob index)
   (when (>=fx index  (blob-length blob))
      (error "blob-u8-ref" "index out of bounds" index))

   (let ((byte (cond ((u8vector? (-> blob data))
                      (u8vector-ref (-> blob data) index))
                     ((string? (-> blob data))
                      (->uint8 (char->integer (string-ref (-> blob data) index))))
                     ((mmap? (-> blob data))
                      (->uint8 ($mmap-ref (-> blob data) index))))))
      (cond-expand
         (bigloo-jvm
          (bit-and #x00ff (uint8->fixnum byte)))
         (else
          (uint8->fixnum byte)))))
   
(define (blob-s8-ref::bint blob::%blob index)
    (let ((byte (cond ((u8vector? (-> blob data))
                      (u8vector-ref (-> blob data) index))
                     ((string? (-> blob data))
                      (->uint8 (char->integer (string-ref (-> blob data) index))))
                     ((mmap? (-> blob data))
                      (->uint8 ($mmap-ref (-> blob data) index))))))
       (int8->fixnum (uint8->int8 byte))))

(define (blob-u8-set! blob::%blob index val::uint8)
   (cond ((u8vector? (-> blob data))
          (u8vector-set! (-> blob data) index val))
         ((string? (-> blob data))
          (string-set! (-> blob data) index  (integer->char (->fixnum val))))
         ((mmap? (-> blob data))
          ($mmap-set! (-> blob data) index (integer->char (->fixnum val))))))

(define (blob-s8-set! blob::%blob index val::int8)
   (cond ((u8vector? (-> blob data))
          (u8vector-set! (-> blob data) index val))
         ((string? (-> blob data))
          (string-set! (-> blob data) index
             (integer->char  (->fixnum  (int8->uint8 val)))))
         ((mmap? (-> blob data))
          ($mmap-set! (-> blob data) index
             (integer->char (->fixnum (int8->uint8 val)))))))

(define (blob-bytes-set! size endianness blob index val)
   (let ((posrep (if (< val 0) (+ (pow256 size) val)
                     val)))
      (cond ((eq? endianness +big-endian+)
             (let loop ((i::long 0)
                        (curr posrep))
                (if (<fx i size)
                    (begin
                       (blob-u8-set! blob (+ index (- size 1 i))  (->uint8 (modulo curr 256)))
                       (loop (+fx i 1)
                          (quotient curr 256)))
                    #unspecified)))
            ((eq? endianness +little-endian+)
             (let loop ((i::long 0)
                        (curr posrep))
                (if (<fx i size)
                    (begin
                       (blob-u8-set! blob (+ index i)  (->uint8 (modulo curr 256)))
                       (loop (+fx i 1)
                          (quotient curr 256)))
                    #unspecified)))
            (else
             (error "blob-bytes-set!" "unkown endianness" endianness)))))

(define (blob-uint-ref size endianness blob::%blob index)
   (cond ((eq? endianness +big-endian+)
          (let loop ((i::long 0)
                     (bm 1)
                     (res 0))
             (if (<fx i size)
                 (loop (+fx i 1)
                    (* bm 256)
                    (+ res (* bm (blob-u8-ref blob (+ index (- size 1 i))))))
                 res)))
         ((eq? endianness +little-endian+)
          (let loop ((i::long 0)
                     (bm 1)
                     (res 0))
             (if (<fx i size)
                 (loop (+fx i 1)
                    (* bm 256)
                    (+ res (* bm (blob-u8-ref blob (+ index i)))))
                  res)))
         (else
          (error "blob-uint-ref" "unkown endianness" endianness))))

(define (blob-uint-set! size endianness blob index val)
   ;; todo add checks on val
   (blob-bytes-set! size endianness blob index val))

(define (blob-sint-ref size endianness blob::%blob index)
   (cond ((eq? endianness +big-endian+)
          (let loop ((i::long 0)
                     (bm 1)
                     (signed #f)
                     (res 0))
             (if (<fx i size)
                 (let ((byte (blob-u8-ref blob (+ index (- size 1 i)))))
                    (loop (+fx i 1)
                       (* bm 256)
                       (>fx byte 127)
                       (+ res (* bm byte))))
                 (if signed  (- 0 (- bm res))  res))))
         ((eq? endianness +little-endian+)
          (let loop ((i::long 0)
                     (bm 1)
                     (signed #f)
                     (res 0))
             (if (<fx i size)
                 (let ((byte (blob-u8-ref blob (+ index i))))
                  (loop (+fx i 1)
                        (* bm 256)
                        (>fx byte 127)
                        (+ res (* bm byte))))
                  (if signed  (- 0 (- bm res)) res))))
         (else
          (error "blob-sint-ref" "unkown endianness" endianness))))

(define (blob-sint-set! size endianness blob index val)
   ;;; todo add checks on val
   (blob-bytes-set! size endianness blob index val))

(define (blob-u16-ref endianness blob index)
   (cond ((eq? endianness +big-endian+)
          (+fx (blob-u8-ref blob (+fx index 1)) (*fx 256 (blob-u8-ref blob index))))
         ((eq? endianness +little-endian+)
          (+fx (blob-u8-ref blob index) (*fx 256 (blob-u8-ref blob (+fx index 1)))))
         (else
          (error "blob-u16-ref" "unknown endianness" endianness))))

(define (blob-s16-ref endianness blob index)
   (cond ((eq? endianness +big-endian+)
          (let* ((byte1 (blob-u8-ref blob (+ index 1)))
                 (byte0 (blob-u8-ref blob index))
                 (res (+fx byte1 (*fx 256 byte0))))
             (if (>fx byte0 127)
                 (-fx 0 (-fx 65536 res))
                 res)))
         ((eq? endianness +little-endian+)
          (let* ((byte1 (blob-u8-ref blob (+ index 1)))
                 (byte0 (blob-u8-ref blob index))
                 (res (+fx byte0 (*fx 256 byte1))))
             (if (>fx byte1 127)
                 (-fx 0 (-fx 65536 res))
                 res)))
         (else
          (error "blob-u16-ref" "unknown endianness" endianness))))

(define (blob-u16-native-ref blob index)
   (blob-u16-ref +native-endian+ blob index))

(define (blob-s16-native-ref blob index)
   (blob-s16-ref +native-endian+ blob index))

(define (blob-2bytes-set! endianness blob index val)
   (let ((posrep (if (< val 0) (+ 65536 val) val)))
      (cond ((eq? endianness +big-endian+)
             (let* ((byte0 (->uint8 (modulo posrep 256)))
                    (byte1 (->uint8 (modulo (quotient posrep 256) 256))))
                (blob-u8-set! blob index byte1)
                (blob-u8-set! blob (+fx index 1) byte0)))
            ((eq? endianness +little-endian+)
             (let* ((byte1 (->uint8 (modulo posrep 256)))
                    (byte0 (->uint8 (modulo (quotient posrep 256) 256))))
                (blob-u8-set! blob index byte1)
                (blob-u8-set! blob (+fx index 1) byte0)))
            (else
             (error "blob-2bytes-set!" "unkown endianness" endianness)))))

(define (blob-u16-set! endianness blob index val)
   (if (and (>= val 0)
           (< val 65536))
       (blob-2bytes-set! endianness blob index val)
       (error "blob-u16-set!" "val is not a valid 16 bit unsigned value" val)))

(define (blob-s16-set! endianness blob index val)
   (if (and (> val -32769)
           (< val 32768))
       (blob-2bytes-set! endianness blob index val)
       (error "blob-s16-set!" "val is not a valid 16 bit signed value" val)))

(define (blob-u16-native-set! blob index val)
   (blob-u16-set! +native-endian+ blob index val))

(define (blob-s16-native-set! blob index val)
   (blob-s16-set! +native-endian+ blob index val))

(define (blob-u32-ref endianness blob index)
   (cond ((eq? endianness +big-endian+)
          (+ (blob-u8-ref blob (+ index 3))
             (* 256 (blob-u8-ref blob (+ index 2)))
             (* 65536 (blob-u8-ref blob (+ index 1)))
             (* #l16777216 (blob-u8-ref blob index))))
         ((eq? endianness +little-endian+)
             (+ (blob-u8-ref blob index)
                (* 256 (blob-u8-ref blob (+ index 1)))
                (* 65536 (blob-u8-ref blob (+ index 2)))
                (* #l16777216 (blob-u8-ref blob (+ index 3)))))
         (else
          (error "blob-u32-ref" "unknown endianness" endianness))))

(define (blob-s32-ref endianness blob index)
   (flush-output-port (current-output-port))
   (cond ((eq? endianness +big-endian+)
          (let* ((byte0 (blob-u8-ref blob (+ index 3)))
                 (byte1 (blob-u8-ref blob (+ index 2)))
                 (byte2 (blob-u8-ref blob (+ index 1)))
                 (byte3 (blob-u8-ref blob index))
                 (res (+ byte0
                         (* 256 byte1)
                         (* 65536 byte2)
                         (* 16777216 byte3))))
             (if (> byte3 127)
                 (- 0 (- #l4294967296 res))
                 res)))
         ((eq? endianness +little-endian+)
          (let* ((byte0 (blob-u8-ref blob index))
                 (byte1 (blob-u8-ref blob (+ index 1)))
                 (byte2 (blob-u8-ref blob (+ index 2)))
                 (byte3 (blob-u8-ref blob (+ index 3)))
                 (res (+ byte0
                         (* 256 byte1)
                         (* 65536 byte2)
                         (* 16777216 byte3))))
             (if (> byte3 127)
                 (- 0 (- #l4294967296 res))
                 res)))
         (else
          (error "blob-s32-ref" "unknown endianness" endianness))))

(define (blob-u32-native-ref blob index)
   (blob-u32-ref +native-endian+ blob index))

(define (blob-s32-native-ref blob index)
   (blob-s32-ref +native-endian+ blob index))

(define (blob-4bytes-set! endianness blob index val)
   (let ((posrep (if (< val 0) (+  #e4294967296 val) val)))
      (cond ((eq? endianness +big-endian+)
          (let* ((byte0 (->uint8 (modulo posrep 256)))
                 (byte1 (->uint8 (modulo (quotient posrep 256) 256)))
                 (byte2 (->uint8 (modulo (quotient posrep 65536) 256)))
                 (byte3 (->uint8 (modulo (quotient posrep 16777216) 256))))
             (blob-u8-set! blob index byte3)
             (blob-u8-set! blob (+ index 1) byte2)
             (blob-u8-set! blob (+ index 2) byte1)
             (blob-u8-set! blob (+ index 3) byte0)))
         ((eq? endianness +little-endian+)
          (let* ((byte3 (->uint8 (modulo posrep 256)))
                 (byte2 (->uint8 (modulo (quotient posrep 256) 256)))
                 (byte1 (->uint8 (modulo (quotient posrep 65536) 256)))
                 (byte0 (->uint8 (modulo (quotient posrep 16777216) 256))))
             (blob-u8-set! blob index byte3)
             (blob-u8-set! blob (+ index 1) byte2)
             (blob-u8-set! blob (+ index 2) byte1)
             (blob-u8-set! blob (+ index 3) byte0)))
         (else
          (error "blob-4bytes-set!" "unkown endianness" endianness)))))

(define (blob-u32-set! endianness blob index val)
   (if (and (>= val #l0)
            (< val #l4294967296))
       (blob-4bytes-set! endianness blob index val)
       (error "blob-u32-set!" "val is not a valid 32 bit unsigned value" val)))

(define (blob-s32-set! endianness blob index val)
   (if (and (> val #l-2147483649)
            (< val #l2147483648))
       (blob-4bytes-set! endianness blob index val)
       (error "blob-s32-set!" "val is not a valid 32 bit signed value" val)))


(define (blob-u32-native-set! blob index val)
   (blob-u32-set! +native-endian+ blob index val))

(define (blob-s32-native-set! blob index val)
   (blob-s32-set! +native-endian+ blob index val))



(define (blob-u64-ref endianness blob index)
   (cond ((eq? endianness +big-endian+)
          (+ (blob-u8-ref blob (+ index 7))
             (* 256 (blob-u8-ref blob (+ index 6)))
             (* 65536 (blob-u8-ref blob (+ index 5)))
             (* 16777216 (blob-u8-ref blob (+ index 4)))
             (* #l4294967296 (blob-u8-ref blob (+ index 3)))
             (* #l1099511627776 (blob-u8-ref blob (+ index 2)))
             (* #l281474976710656 (blob-u8-ref blob (+ index 1)))
             (* #z72057594037927936 (blob-u8-ref blob index))))
         ((eq? endianness +little-endian+)
             (+ (blob-u8-ref blob index)
             (* 256 (blob-u8-ref blob (+ index 1)))
             (* 65536 (blob-u8-ref blob (+ index 2)))
             (* 16777216 (blob-u8-ref blob (+ index 3)))
             (* #l4294967296 (blob-u8-ref blob (+ index 4)))
             (* #l1099511627776 (blob-u8-ref blob (+ index 5)))
             (* #l281474976710656 (blob-u8-ref blob (+ index 6)))
             (* #z72057594037927936 (blob-u8-ref blob (+ index 7)))))
         (else
          (error "blob-u64-ref" "unknown endianness" endianness))))

(define (blob-s64-ref endianness blob index)
   (cond ((eq? endianness +big-endian+)
          (let* ((byte0 (blob-u8-ref blob (+ index 7)))
                 (byte1 (blob-u8-ref blob (+ index 6)))
                 (byte2 (blob-u8-ref blob (+ index 5)))
                 (byte3 (blob-u8-ref blob (+ index 4)))
                 (byte4 (blob-u8-ref blob (+ index 3)))
                 (byte5 (blob-u8-ref blob (+ index 2)))
                 (byte6 (blob-u8-ref blob (+ index 1)))
                 (byte7 (blob-u8-ref blob index))
                 (res (+ byte0
                         (* 256 byte1)
                         (* 65536 byte2)
                         (* 16777216 byte3)
                         (* #l4294967296 byte4)
                         (* #l1099511627776 byte5)
                         (* #l281474976710656 byte6)
                         (* #z72057594037927936 byte7))))
             (if (> byte7 127)
                 (- 0 (- #z18446744073709551616 res))
                 res)))
         ((eq? endianness +little-endian+)
          (let* ((byte0 (blob-u8-ref blob index))
                 (byte1 (blob-u8-ref blob (+ index 1)))
                 (byte2 (blob-u8-ref blob (+ index 2)))
                 (byte3 (blob-u8-ref blob (+ index 3)))
                 (byte4 (blob-u8-ref blob (+ index 4)))
                 (byte5 (blob-u8-ref blob (+ index 5)))
                 (byte6 (blob-u8-ref blob (+ index 6)))
                 (byte7 (blob-u8-ref blob (+ index 7)))
                 (res (+ byte0
                         (* 256 byte1)
                         (* 65536 byte2)
                         (* 16777216 byte3)
                         (* #l4294967296 byte4)
                         (* #l1099511627776 byte5)
                         (* #l281474976710656 byte6)
                         (* #z72057594037927936 byte7))))
            
             (if (> byte7 127)
                 (- 0 (- #z18446744073709551616 res))
                 res)))
         (else
          (error "blob-s64-ref" "unknown endianness" endianness))))


(define (blob-u64-native-ref blob index)
   (blob-u64-ref +native-endian+ blob index))

(define (blob-s64-native-ref blob index)
   (blob-s64-ref +native-endian+ blob index))


(define (blob-8bytes-set! endianness blob index val)
   (let ((posrep (if (< val 0) (+  #z18446744073709551616 val) val)))
      (cond ((eq? endianness +big-endian+)
             (let* ((byte0 (->uint8 (modulo posrep 256)))
                    (byte1 (->uint8 (modulo (quotient posrep 256) 256)))
                    (byte2 (->uint8 (modulo (quotient posrep 65536) 256)))
                    (byte3 (->uint8 (modulo (quotient posrep 16777216) 256)))
                    (byte4 (->uint8 (modulo (quotient posrep #l4294967296) 256)))
                    (byte5 (->uint8 (modulo (quotient posrep #l1099511627776) 256)))
                    (byte6 (->uint8 (modulo (quotient posrep #l281474976710656) 256)))
                    (byte7 (->uint8 (modulo (quotient posrep #z72057594037927936) 256))))
                (blob-u8-set! blob index byte7)
                (blob-u8-set! blob (+ index 1) byte6)
                (blob-u8-set! blob (+ index 2) byte5)
                (blob-u8-set! blob (+ index 3) byte4)
                (blob-u8-set! blob (+ index 4) byte3)
                (blob-u8-set! blob (+ index 5) byte2)
                (blob-u8-set! blob (+ index 6) byte1)
                (blob-u8-set! blob (+ index 7) byte0)))
            ((eq? endianness +little-endian+)
             (let* ((byte7 (->uint8 (modulo posrep 256)))
                    (byte6 (->uint8 (modulo (quotient posrep 256) 256)))
                    (byte5 (->uint8 (modulo (quotient posrep 65536) 256)))
                    (byte4 (->uint8 (modulo (quotient posrep 16777216) 256)))
                    (byte3 (->uint8 (modulo (quotient posrep #l4294967296) 256)))
                    (byte2 (->uint8 (modulo (quotient posrep #l1099511627776) 256)))
                    (byte1 (->uint8 (modulo (quotient posrep #l281474976710656) 256)))
                    (byte0 (->uint8 (modulo (quotient posrep #z72057594037927936) 256))))
                (blob-u8-set! blob index byte7)
                (blob-u8-set! blob (+ index 1) byte6)
                (blob-u8-set! blob (+ index 2) byte5)
                (blob-u8-set! blob (+ index 3) byte4)
                (blob-u8-set! blob (+ index 4) byte3)
                (blob-u8-set! blob (+ index 5) byte2)
                (blob-u8-set! blob (+ index 6) byte1)
                (blob-u8-set! blob (+ index 7) byte0)))
         (else
          (error "blob-8bytes-set!" "unkown endianness" endianness)))))


(define (blob-u64-set! endianness blob index val)
   (if (and (>= val 0)
            (< val #z18446744073709551616))
       (blob-8bytes-set! endianness blob index val)
       (error "blob-u64-set!" "val is not a valid 64 bit unsigned value" val)))

(define (blob-s64-set! endianness blob index val)
   (if (and (> val #z-9223372036854775809)
            (< val #z9223372036854775808))
       (blob-8bytes-set! endianness blob index val)
       (error "blob-s32-set!" "val is not a valid 64 bit signed value" val)))

(define (blob-u64-native-set! blob index val)
   (blob-u64-set! +native-endian+ blob index val))

(define (blob-s64-native-set! blob index val)
   (blob-s64-set! +native-endian+ blob index val))

(define (blob-f32-set! blob index val::float)
   (let ((byterep (float->int-bits val)))
      (blob-u32-set! +big-endian+ blob index byterep)))

(define (blob-f32-ref blob index)
   (let ((byterep (->fixnum (blob-u32-ref +big-endian+ blob index))))
      (int-bits->float  byterep)))

(define (blob-f64-set! blob index val::double)
   (let ((byterep (double->llong-bits val)))
      (blob-u64-set! +big-endian+ blob index byterep)))

(define (blob-f64-ref blob index)
   (let ((byterep  (->llong (blob-u64-ref +big-endian+ blob index))))
      (llong-bits->double byterep)))


(define (blob->u8-list blob::%blob)
   (cond ((u8vector? (-> blob data))
          (map! (lambda (v) (if (< v 0) (+ 256 v) v)) (u8vector->list (-> blob data))))
         ((string? (-> blob data))
          (map! (lambda (v) (let ((b (char->integer v))) (if (< b 0) (+ 256 b) b)))
             (string->list (-> blob data))))
         ((mmap? (-> blob data))
          (map! (lambda (v) (let ((b (char->integer v))) (if (< b 0) (+ 256 b) b)))
             (string->list  (blob->string blob))))
         (else
          (error "blob->u8-list" "unkown blob type" blob))))

(define (u8-list->blob lst)
   (if (every byte? lst)
       (instantiate::%blob (data (list->u8vector lst)))
       (error "u8-list->blob" "all list items must be a byte (0 <= n < 256)" lst)))

(define (blob->uint-list size endianness blob)
   (let loop ((i::long 0)
              (res '()))
      (if (<fx i (blob-length blob))
          (loop (+fx i size)
             (cons (blob-uint-ref size endianness blob i) res))
          (reverse! res))))

(define (blob->sint-list size endianness blob)
   (let loop ((i::long 0)
              (res '()))
      (if (<fx i (blob-length blob))
          (loop (+fx i size)
             (cons (blob-sint-ref size endianness blob i) res))
          (reverse! res))))


(define (uint-list->blob size endianness lst)
   (let ((len (length lst))
         (res (make-blob (* size (length lst)))))
      (let loop ((i::long 0)
                 (curr lst))
         (if (<fx i len)
             (begin
                (blob-uint-set! size endianness res (* i size) (car curr))
                (loop (+fx i 1)
                   (cdr curr)))
             res))))

(define (sint-list->blob size endianness lst)
   (let* ((len (length lst))
          (res (make-blob (* size len))))
      (let loop ((i::long 0)
                 (curr lst))
         (if (<fx i len)
             (begin
                (blob-sint-set! size endianness res (* i size) (car curr))
                (loop (+ i 1)
                   (cdr curr)))
             res))))


(define (blob->f32-list blob)
   (let loop ((i::long 0)
              (res '()))
      (if (<fx i (blob-length blob))
          (loop (+fx i 4)
             (cons (blob-f32-ref blob i) res))
          (reverse! res))))

(define (f32-list->blob lst)
   (let* ((len (length lst))
          (res (make-blob (* 4 len))))
      (let loop  ((i::long 0)
                  (curr lst))
         (if (<fx i len)
             (begin
                (blob-f32-set! res (* i 4) (car curr))
                (loop (+fx i 1)
                   (cdr curr)))
             res))))

(define (blob->f64-list blob)
   (let loop ((i::long 0)
              (res '()))
      (if (<fx i (blob-length blob))
          (loop (+fx i 8)
             (cons (blob-f64-ref blob i) res))
          (reverse! res))))

(define (f64-list->blob lst)
   (let* ((len (length lst))
          (res (make-blob (* 8 len))))
      (let loop  ((i::long 0)
                  (curr lst))
         (if (<fx i len)
             (begin
                (blob-f64-set! res (* i 8) (car curr))
                (loop (+fx i 1)
                   (cdr curr)))
             res))))

(define (string->blob str)
   (instantiate::%blob (data (string-copy str))))

(define (string->blob! str)
   (instantiate::%blob (data str)))

(define (blob->string blob::%blob)
   (cond ((string? (-> blob data))
          (-> blob data))
         (else
          (do ((i::long 0 (+fx i 1))
               (res (make-string (blob-length blob))))
              ((=fx i (blob-length blob)) res)
              (string-set! res i (integer->char (blob-u8-ref blob i)))))))

(define (u8vector->blob::%blob vec::u8vector)
   (instantiate::%blob (data vec)))

(define (mmap->blob::%blob map::mmap)
   (instantiate::%blob (data map)))


(define (blob-str-ref blob::%blob index::long length::long)
   (if (<= (+ index length) (blob-length blob))
       (do ((i 0 (+fx i 1))
            (res (make-string length)))
           ((=fx i length) res)
           (string-set! res i (integer->char (blob-u8-ref blob (+fx index i)))))
       (error "blob-str-ref" "index or length out of bounds"
          `(index ,index length ,length))))

(define (blob-str-set! blob::%blob index::long str::bstring)
   (let ((len (string-length str)))
      (if (<= (+ index len) (blob-length blob))
          (do ((i 0 (+fx i 1)))
              ((=fx i len))
              (blob-u8-set! blob (+ index i)
                 (char->integer (string-ref str i))))
          (error "blob-str-set!" "erroneous index or string length"
             `(index ,index str ,str)))))

(define-inline (find-cstr-length::long blob::%blob index::long)
   (let loop ((i::long 0))
      (if (< (+fx index i) (blob-length blob))
          (if (= (blob-u8-ref blob (+fx index i)) 0)
              i
              (loop (+fx i 1)))
          (error "find-cstr-length" "could not find nul char" index))))

(define (blob-cstr-ref blob::%blob index::long)
   (let ((strlen (find-cstr-length blob index)))
      (blob-str-ref blob index strlen)))


(define-method (object-equal? obj::%blob b)
   (and (blob? b)
        (blob=? obj b)))      

