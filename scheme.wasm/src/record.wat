
;; (define-record-type <name> <constructor> <predicate> <field> ...)
;; <constructor> := (<contructor name> <field name> ...)
;; <field> := (<field name> <accessor name> <modifier name> )
(func $define-record-type (param $env i32) (param $args i32) (result i32)
  (local $num-args i32)
  (local $temp i32)
  (local $name i32)
  (local $constructor i32)
  (local $predicate i32)
  (local $fields i32)
  (local $record-type i32)
  (local $field i32)
  (local $accessor i32)
  (local $modifier i32)
  (local $index i32)

  (block $check (block $fail
      (local.set $num-args (call $list-len (local.get $args)))
      (br_if $fail (i32.le_u (local.get $num-args) (i32.const 3)))

      (local.set $temp (local.get $args))

      (%pop-l $name $temp)
      (%chk-type $fail $name %symbol-type)
      (br_if $fail (call $environment-has (local.get $env) (local.get $name)))

      (%pop-l $constructor $temp)
      (%chk-type $fail $constructor %cons-type)

      (br_if $fail (i32.eqz (call $valid-record-type-constructor?
            (local.get $constructor))))

      (%pop-l $predicate $temp)
      (%chk-type $fail $predicate %symbol-type)

      (local.set $fields (local.get $temp))
      (br_if $fail (i32.eqz (call $valid-record-type-fields?
            (local.get $env)
            (local.get $constructor)
            (local.get $predicate)
            (local.get $fields))))

      (br $check))

    (return (call $argument-error (local.get $args))))

  ;; create the record-type
  (local.set $record-type (call $heap-alloc
      (global.get $g-heap)
      (%record-meta-type)
      (local.get $args)
      (i32.const 0)))
  ;; set the record type in the environment
  (call $environment-add
    (local.get $env)
    (local.get $name)
    (local.get $record-type))

  (%define %alloc-record-method (%method %index) (call $heap-alloc
      (global.get $g-heap)
      (%record-method-type)
      (local.get $record-type)
      (i32.or (i32.shl (i32.const %method) (i32.const 24)) (local.get %index))))

  (local.set $index (i32.const 0))
  ;; create the constructor
  (call $environment-add
    (local.get $env)
    (%car-l $constructor)
    (%alloc-record-method 0 $index))
  ;; create the predicate
  (call $environment-add
    (local.get $env)
    (local.get $predicate)
    (%alloc-record-method 1 $index))

  ;; Walk the fields and create the accessors and modifiers
  (%inc $index)
  (block $end (loop $start
      (br_if $end (i32.eq (local.get $fields) (global.get $g-nil)))
      (%pop-l $field $fields)
      ;; skip the field name
      (local.set $field (%cdr-l $field))
      (%pop-l $accessor $field)
      (call $environment-add
        (local.get $env)
        (local.get $accessor)
        (%alloc-record-method 2 $index))
      (if (i32.ne (local.get $field) (global.get $g-nil)) (then
        (%pop-l $modifier $field)
        (call $environment-add
          (local.get $env)
          (local.get $modifier)
          (%alloc-record-method 3 $index))))
      (%inc $index)
      (br $start)))

  (return (global.get $g-nil)))

;; <constructor> := (<constructor name> <field name> ...)
(func $valid-record-type-constructor? (param $ctor i32) (result i32)
  (local $name i32)

  ;; constructor must be a list
  (if (i32.eqz (call $is-list-impl (local.get $ctor))) (then
      (return (i32.const 0))))

  ;; verify that name is a symbol
  (local.set $name (%car-l $ctor))
  (if (i32.ne (%get-type $name) (%symbol-type)) (then
      (return (i32.const 0))))

  ;; verify that field names are unique
  (return (call $list-of-unique-symbols? (%cdr-l $ctor))))

;; every field in the constructor must have a field accessor
;; accessors and modifiers must be unique and definable within the current
;; environment
;;
;; first walk through the fields in the ctor and verify that there is a corresponding
;; accessor.
;; Walk through fields and create list of field names
;;    check this for uniqueness
;; also create list of accessors and modifiers (and predicate and ctor)
;;    check for uniqueness.
;;    check for definability.
(func $valid-record-type-fields? (param $env i32) (param $ctor i32) (param $predicate i32) (param $fields i32) (result i32)
  (local $temp i32)
  (local $curr i32)
  (local $field i32)
  (local $count i32)
  (local $field_names i32)
  (local $accessors i32)
  (local $ctor-name i32)
  (local $accessor i32)
  (local $modifier i32)

  ;; check that fields is a list of pairs, we'll check deeper structure lower
  ;; down
  (if (i32.eqz (call $all-cons (local.get $fields))) (then
      (return (i32.const 0))))

  ;; Check that each field in the ctor has a corresponding element in the fields
  ;; list
  (local.set $temp (%cdr-l $ctor))
  (block $end (loop $start
      (br_if $end (i32.eq (local.get $temp) (global.get $g-nil)))
      (%pop-l $curr $temp)
      (local.set $field (call $assq-impl (local.get $curr) (local.get $fields)))
      (if (i32.eq (local.get $field) (global.get $g-false)) (then
          ;; there isn't a corresponding field definition
          (return (i32.const 0))))
      (br $start)))

  ;; check that the predicate and ctor name aren't the same
  (local.set $ctor-name (%car-l $ctor))
  (if (i32.eq (local.get $ctor-name) (local.get $predicate)) (then
      (return (i32.const 0))))
  ;; check that the predicate and ctor name are defineable
  (if (call $environment-has (local.get $env) (local.get $ctor-name)) (then
      (return (i32.const 0))))
  (if (call $environment-has (local.get $env) (local.get $predicate)) (then
      (return (i32.const 0))))

  (local.set $field_names (call $hashtable-init (i32.const 0)))
  (local.set $accessors (call $hashtable-init (i32.const 0)))

  ;; Add the ctor name and the predicate to the accessor names
  (local.set $accessors (call $hashtable-add
      (local.get $accessors)
      (local.get $ctor-name)
      (i32.const 1)))
  (local.set $accessors (call $hashtable-add
      (local.get $accessors)
      (local.get $predicate)
      (i32.const 1)))


  ;; walk the list of fields and check field names for uniqueness, also check
  ;; accessor and modifier for uniqueness, and definability
  (local.set $temp (local.get $fields))
  (block $end (block $fail (loop $start
          (br_if $end (i32.eq (local.get $temp) (global.get $g-nil)))
          (%pop-l $curr $temp)
          ;; check that the field definition has the correct structure
          (block $check
            (br_if $fail (i32.eqz (call $is-list-impl (local.get $curr))))
            (br_if $fail (i32.eqz (call $all-symbol (local.get $curr))))
            (local.set $count (call $list-len (local.get $curr)))
            (br_if $fail (i32.lt_u (local.get $count) (i32.const 2)))
            (br_if $fail (i32.gt_u (local.get $count) (i32.const 3)))
            (br $check))

          ;; check that the field name is unique
          (br_if $fail (call $hashtable-has (local.get $field_names) (%car-l $curr)))
          (local.set $field_names (call $hashtable-add
              (local.get $field_names)
              (%car-l $curr)
              (i32.const 1)))

          ;; check that the accessor is unique
          (local.set $accessor (%car (%cdr-l $curr)))
          (br_if $fail (call $hashtable-has (local.get $accessors) (local.get $accessor)))
          (local.set $accessors (call $hashtable-add
              (local.get $accessors)
              (local.get $accessor)
              (i32.const 1)))

          ;; check that the accessor is definable
          (br_if $fail (call $environment-has (local.get $env) (local.get $accessor)))

          ;; if there is a modifier
          (if (i32.eq (local.get $count) (i32.const 3)) (then
              ;; check that the modifier is unique
              (local.set $modifier (%car (%cdr (%cdr-l $curr))))
              (br_if $fail (call $hashtable-has (local.get $accessors) (local.get $modifier)))
              (local.set $accessors (call $hashtable-add
                  (local.get $accessors)
                  (local.get $modifier)
                  (i32.const 1)))))

              ;; check that the modifier is definable
              (br_if $fail (call $environment-has (local.get $env) (local.get $modifier)))


      (br $start)))

    ;; This is the fail block, cleanup and return 0
    (call $malloc-free (local.get $field_names))
    (call $malloc-free (local.get $accessors))
    (return (i32.const 0)))

  ;; This is the success block, return 1
  (call $malloc-free (local.get $field_names))
  (call $malloc-free (local.get $accessors))

  (return (i32.const 1)))

;; The easiest way to check this is to stuff the symbols into a hashtable and
;; check that there are no collisions
(func $list-of-unique-symbols? (param $list i32) (result i32)
  (local $hashtable i32)
  (local $curr i32)
  (local $curr-ptr i32)

  (local.set $hashtable (call $hashtable-init (i32.const 0)))

  (block $end (loop $start
      (br_if $end (i32.eq (local.get $list) (global.get $g-nil)))
      (local.set $curr (%car-l $list))
      (%chk-type $end $curr %symbol-type)

      (local.set $curr-ptr (%car-l $curr))
      (br_if $end (call $hashtable-has
          (local.get $hashtable)
          (local.get $curr-ptr)))
      (local.set $hashtable (call $hashtable-add
          (local.get $hashtable)
          (local.get $curr-ptr)
          (local.get $curr-ptr)))

      (local.set $list (%cdr-l $list))
      (br $start)))

  (call $malloc-free (local.get $hashtable))

  (return (select
      (i32.const 1)
      (i32.const 0)
      (i32.eq (local.get $list) (global.get $g-nil)))))

(func $apply-record-method (param $method i32) (param $args i32) (result i32)
  (local $record-type i32)
  (local $method-type i32)
  (local $method-index i32)

  (local.set $record-type (%car-l $method))
  (local.set $method-type (i32.shr_u (%cdr-l $method) (i32.const 24)))

  (if (i32.eqz (local.get $method-type)) (then
      ;; This is a constructor.
      (return (call $apply-record-ctor
          (local.get $record-type)
          (local.get $args)))))

  (if (i32.eq (local.get $method-type) (i32.const 1)) (then
      ;; This is the predicate.
      (return (call $apply-record-predicate
          (local.get $record-type)
          (local.get $args)))))

  (local.set $method-index (i32.and (%cdr-l $method) (i32.const 0x00FF_FFFF)))

  (if (i32.eq (local.get $method-type) (i32.const 2)) (then
      ;; This is an accessor.
      (return (call $apply-record-get
          (local.get $record-type)
          (local.get $method-index)
          (local.get $args)))))

  (if (i32.eq (local.get $method-type) (i32.const 3)) (then
      ;; This is a modifier.
      (return (call $apply-record-set
          (local.get $record-type)
          (local.get $method-index)
          (local.get $args)))))

  (unreachable))

;; Ctor definition is 2nd parameter of the record type definition
;; (ctor-name field-name ...)
;; Fields are the 4th parameter onwards of the record type definition.
;; We need to fields to look up the indices for each ctor argument.
(func $apply-record-ctor (param $record-type i32) (param $args i32) (result i32)
  (local $ctor-def i32)
  (local $ctor-field i32)
  (local $fields i32)
  (local $count i32)
  (local $index i32)
  (local $temp i32)
  (local $curr i32)
  (local $vector i32)
  (local $record i32)
  (local $arg i32)
  (local $field i32)

  (local.set $temp (%car-l $record-type))
  ;; pop the name (which we don't use)
  (%pop-l $curr $temp)
  ;; pop the ctor-definition
  (%pop-l $ctor-def $temp)
  ;; pop the predicate (which we don't use)
  (%pop-l $curr $temp)
  ;; fields are the remaining parameters
  (local.set $fields (local.get $temp))

  ;; There need to be exactly as many arguments as there are ctor fields
  (local.set $ctor-field (%cdr-l $ctor-def))
  (if (i32.ne
      (call $list-len (local.get $ctor-field))
      (call $list-len (local.get $args)))
    (then
      (return (call $argument-error (local.get $args)))))

  ;; vector count is the number of fields plus one for the type
  (local.set $count (i32.add (call $list-len (local.get $fields)) (i32.const 1)))
  ;; allocate a (zero-initialized) vector of the correct size
  (local.set $vector (call $calloc (local.get $count) (i32.const 4)))
  ;; set up the record
  (local.set $record (call $heap-alloc
      (global.get $g-heap)
      (%record-type)
      (local.get $vector)
      (local.get $count)))
  ;; initialize the type
  (i32.store (local.get $vector) (local.get $record-type))

  ;; walk the ctor fields and args
  (block $end (loop $start
      (br_if $end (i32.eq (local.get $ctor-field) (global.get $g-nil)))
      (br_if $end (i32.eq (local.get $args) (global.get $g-nil)))

      (%pop-l $arg $args)
      (%pop-l $field $ctor-field)
      ;; find the index of the field
      (local.set $index (call $index-of (local.get $fields) (local.get $field)))
      ;; store the arg in the relevant slot
      (i32.store offset=4
        (i32.add (local.get $vector) (%word-size-l $index))
        (local.get $arg))

      (br $start)))

  (return (local.get $record)))

(func $index-of (param $fields i32) (param $field i32) (result i32)
  (local $curr i32)
  (local $index i32)

  (local.set $index (i32.const 0))
  (block $end (loop $start
      (br_if $end (i32.eq (local.get $fields) (global.get $g-nil)))
      (%pop-l $curr $fields)
      (if (i32.eq (%car-l $curr) (local.get $field)) (then
          (return (local.get $index))))

      (%inc $index)
      (br $start)))

  (unreachable))

;; Validates that the single argument is a record of the correct type
(func $apply-record-predicate (param $record-type i32) (param $args i32) (result i32)
  (local $record i32)

  (if (i32.ne (call $list-len (local.get $args)) (i32.const 1)) (then
      (return (call $argument-error (local.get $args)))))

  (local.set $record (%car-l $args))

  (return (select
      (global.get $g-true)
      (global.get $g-false)
      (call $record-predicate (local.get $record-type) (local.get $record)))))

;; Implements the validation that an object is a record of the correct type
(func $record-predicate (param $record-type i32) (param $record i32) (result i32)
  (if (i32.ne (%get-type $record) (%record-type)) (then
      (return (i32.const 0))))

  (return (select
      (i32.const 1)
      (i32.const 0)
      (i32.eq (local.get $record-type) (i32.load (%car-l $record))))))

;; Implements a record field accessor
(func $apply-record-get (param $record-type i32) (param $index i32) (param $args i32) (result i32)
  (local $record i32)

  (block $check (block $fail
      (br_if $fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $record (%car-l $args))
      (br_if $fail (i32.eqz (call $record-predicate
            (local.get $record-type)
            (local.get $record))))
      (br $check))

    (return (call $argument-error (local.get $args))))

  (%assert (i32.lt_u (local.get $index) (%cdr-l $record)))

  (return (i32.load (i32.add (%car-l $record) (%word-size-l $index)))))

;; Implements a record field modifier
(func $apply-record-set (param $record-type i32) (param $index i32) (param $args i32) (result i32)
  (local $record i32)
  (local $value i32)

  (block $check (block $fail
      (br_if $fail (i32.ne (call $list-len (local.get $args)) (i32.const 2)))
      (local.set $record (%car-l $args))
      (br_if $fail (i32.eqz (call $record-predicate
            (local.get $record-type)
            (local.get $record))))
      (local.set $value (%car (%cdr-l $args)))
      (br $check))

    (return (call $argument-error (local.get $args))))

  (%assert (i32.lt_u (local.get $index) (%cdr-l $record)))

  (i32.store
    (i32.add (%car-l $record) (%word-size-l $index))
    (local.get $value))
  (return (global.get $g-nil)))

;; (record->list <record>)
;; Converts a record object to an associated list of field name-values
(func $record->list (param $env i32) (param $args i32) (result i32)
  (local $record i32)
  (local $count i32)
  (local $ptr i32)
  (local $record-type i32)
  (local $head i32)
  (local $fields i32)
  (local $field i32)
  (local $value i32)

  (block $check (block $fail
      (br_if $fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $record (%car-l $args))
      (%chk-type $fail $record %record-type)
      (br $check))

    (return (call $argument-error (local.get $args))))

  (local.set $ptr (%car-l $record))
  (local.set $count (%cdr-l $record))

  ;; first entry is the type
  (local.set $record-type (%car (i32.load (local.get $ptr))))
  (%dec $count)
  (%plus-eq $ptr 4)

  (local.set $head (%alloc-list-1 (%alloc-list-1 (%car-l $record-type))))
  (local.set $fields (%cdr (%cdr (%cdr-l $record-type))))

  ;; loop over the fields, adding them to the list if they are defined
  (block $end (loop $start
      (br_if $end (i32.eqz (local.get $count)))
      (br_if $end (i32.eq (local.get $fields) (global.get $g-nil)))

      (%pop-l $field $fields)
      ;; If value is 0, then the field is not defined in the record, this
      ;; happens when a field isn't part of the ctor list of fields and
      ;; hasn't had a value assigned to it.
      (if (local.tee $value (i32.load (local.get $ptr)))
        (then
          (local.set $head (%alloc-cons
              (%alloc-list-2 (%car-l $field) (local.get $value))
              (local.get $head))))
        (else
          ;; just add the field name for empty fields.
          (local.set $head (%alloc-cons
              (%alloc-list-1 (%car-l $field))
              (local.get $head)))))

      (%dec $count)
      (%plus-eq $ptr 4)
      (br $start)))

  (return (call $reverse-impl (local.get $head))))
