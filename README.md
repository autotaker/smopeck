# smopeck
Mocking and Specification Tool for API prototyping

# How to compile
cabal new-build

# How to run
cabal new-exec smopeck CMD ARGS

# Features
## `smopeck mock`
This command launch a mock server accoding to spec file.

### Example
```
$ smopeck mock --host localhost --port 8888 example/hello.spec &
$ curl http://localhost:8888/hello
"hello world!"
```

## `smopeck test`
This command generates a random value of `Main` type in the spec file.

### Example
```
$ smopeck test example/simple.spec | jq .
{
  "intField2": 62,
  "booleanField": false,
  "numberField": -59.61538462071759,
  "stringField": "1552532052",
  "intField1": 64,
  "arrayField": [
    {
      "snd": "fuga",
      "fst": 30
    },
    {
      "snd": "hoge",
      "fst": 31
    },
    {
      "snd": "hoge",
      "fst": 32
    }
  ],
  "nullField": null
}
```