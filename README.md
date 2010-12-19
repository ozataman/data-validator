# Data.Validator

## Introduction
This library is very much experimental at this point, but is meant for
practical real-world data validation applications.


Any feedback is welcome.


## TODO
* Make included validators as polymorphic as possible. Currently, the included
  validator combinators are meant to be used for inputs that are based on
  a single ByteString value. This can bee observed from their types that read
  "FieldValidator m ByteString a". 
