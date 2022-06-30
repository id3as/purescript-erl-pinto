-- | The functions in this module are highly unsafe as they treat records like
-- | atom-keyed maps and can coerce the row of labels that a record has.
-- |
-- | These function are intended for situations where there is some other way of
-- | proving things about the structure of the record - for example, when using
-- | `RowToList`. **They should never be used for general record manipulation.**
module Erl.Record.Unsafe where

import Erl.Atom (Atom)

-- | Checks if a record has a key, using a string for the key.
foreign import unsafeHas :: forall r1. Atom -> Record r1 -> Boolean

-- | Unsafely gets a value from a record, using a string for the key.
-- |
-- | If the key does not exist this will cause a runtime error elsewhere.
foreign import unsafeGet :: forall r a. Atom -> Record r -> a

-- | Unsafely sets a value on a record, using a string for the key.
-- |
-- | The output record's row is unspecified so can be coerced to any row. If the
-- | output type is incorrect it will cause a runtime error elsewhere.
foreign import unsafeSet :: forall r1 r2 a. Atom -> a -> Record r1 -> Record r2

-- | Unsafely removes a value on a record, using a string for the key.
-- |
-- | The output record's row is unspecified so can be coerced to any row. If the
-- | output type is incorrect it will cause a runtime error elsewhere.
foreign import unsafeDelete :: forall r1 r2. Atom -> Record r1 -> Record r2
