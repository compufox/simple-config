# simple-config
### _ava fox_

a library to load and parse simple config files of the format KEY = VALUE

attempts to parse each value into its proper lisp data type.

config example:
```
env = development
program-version=2
aws_bucket = config-bucket
```

## Install

`(ql:quickload :simple-config)`

## API

`load-config` - file-path **&key** parse-lists list-separator

Loads a config file from FILE-PATH

if PARSE-LISTS is non-nil VALUES are checked for LIST-SEPARATOR and split by it.


PARSE-LISTS defaults to t

LIST-SEPARATOR defaults to #\\,

Returns t if successfully loaded, otherwise nil

---

`config` - key **&optional** default

Checks loaded config for KEY, returning nil if not found

If DEFAULT is given, returns it instead of nil if KEY isn't found

--- 

## License

BSD 3-Clause

